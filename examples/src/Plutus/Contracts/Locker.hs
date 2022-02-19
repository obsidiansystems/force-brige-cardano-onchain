{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Plutus.Contracts.Locker where

import GHC.Generics
import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Ledger                (Address, Datum (Datum), ScriptContext, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Address
import           Ledger.Tx             (ChainIndexTxOut (..))
import qualified Ledger.Typed.Scripts  as Scripts
-- import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contracts.Bridge as Bridge
import           Plutus.Contract.Trace as X
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import           Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Builtins.Class as Builtins
import qualified Prelude as Haskell (Semigroup (..), Show, foldMap)
import Data.Aeson (FromJSON, ToJSON)

type CKBAddress = BuiltinByteString

type LockerSchema =
  Endpoint "lock" LockParams
  .\/ Endpoint "unlock" UnlockParams

data MultiSig =
  MultiSig
      { signatories      :: [Ledger.PaymentPubKeyHash]
      -- ^ List of public keys of people who may sign the transaction
      , minNumSignatures :: Integer
      -- ^ Minimum number of signatures required to unlock
      --   the output (should not exceed @length signatories@)
      } deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { ckbAddress :: CKBAddress
    , amount     :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

data UnlockParams = UnlockParams
    { unlockAddress :: CKBAddress
    , multiSig :: (MultiSig, [PaymentPubKeyHash])
    , unlockValue :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''LockParams

data Locker
instance Scripts.ValidatorTypes Locker where
  type instance RedeemerType Locker = LockParams
  type instance DatumType Locker = BuiltinByteString

validateLock :: BuiltinByteString -> LockParams -> ScriptContext -> Bool
validateLock hs cs _ = True

lockerInstance :: Scripts.TypedValidator Locker
lockerInstance = Scripts.mkTypedValidator @Locker
    $$(PlutusTx.compile [|| validateLock ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @BuiltinByteString @LockParams

lockerAddress :: Address
lockerAddress = Scripts.validatorAddress lockerInstance

-- TODO(skylar): Validate CKB Address
-- | The "lock" contract endpoint. See note [Contract endpoints]
lock :: AsContractError e => Promise () LockerSchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams ckbAddr amt) -> do
    logInfo @Haskell.String $ "Locking ada to send to " <> (C.unpack $ Builtins.fromBuiltin ckbAddr)
    let tx         = Constraints.mustPayToTheScript ckbAddr amt
    void (submitTxConstraints lockerInstance tx)

unlock :: AsContractError e => Promise () LockerSchema e ()
unlock = endpoint @"unlock" @UnlockParams $ \up@(UnlockParams ckbAddr (_, pks) v) -> do
  bridgeUtxos <- utxosAt Bridge.bridgeAddress
  utxos <- fundsAtAddressGeq lockerAddress v
  let
    redeemer = LockParams ckbAddr v
    tx = collectFromScript utxos redeemer <> foldMap Constraints.mustBeSignedBy pks
    verified = maybe False (flip verifyVerifiers pks) $ findVerifierInfo bridgeUtxos
  case verified of
    True ->
      void $ submitTxConstraintsSpending lockerInstance utxos tx
    False ->
      return ()

verifyVerifiers :: Bridge.VerifierInfo -> [Ledger.PaymentPubKeyHash] -> Bool
verifyVerifiers (Bridge.VerifierInfo hashes threshold) input = count >= threshold
  where
    count = length $ filter (flip elem hashes) input

-- TODO(skylar): Move to bridge
findVerifierInfo :: Map Ledger.TxOutRef ChainIndexTxOut -> Maybe Bridge.VerifierInfo
findVerifierInfo =
  listToMaybe . catMaybes . Map.elems . Map.map verifierInfo

verifierInfo :: ChainIndexTxOut -> Maybe Bridge.VerifierInfo
verifierInfo o = do
  Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
  PlutusTx.fromBuiltinData d

locker :: AsContractError e => Contract () LockerSchema e ()
locker = do
    logInfo @Haskell.String "Waiting for lock endpoint..."
    selectList [lock] >> locker

lockTrace :: Wallet -> Haskell.String -> EmulatorTrace ()
lockTrace wallet secretWord = do
    hdl <- Trace.activateContractWallet wallet (lock @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"lock" hdl (LockParams "ckb1qyqrdsefa43s6m882pcj53m4gdnj4k440axqdt9rtd" (Ada.adaValueOf 10))
    void $ Trace.waitNSlots 1
