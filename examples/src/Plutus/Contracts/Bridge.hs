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


module Plutus.Contracts.Bridge where

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

data VerifierInfo =
  VerifierInfo { keyhashes :: [Ledger.PaymentPubKeyHash]
               , threshold :: Haskell.Integer
               }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''VerifierInfo

type BridgeSchema =
  Endpoint "verifiers" VerifierInfo

data Bridge
instance Scripts.ValidatorTypes Bridge where
  type instance RedeemerType Bridge = VerifierInfo
  type instance DatumType Bridge = VerifierInfo

-- | The address of the bridge (the hash of its validator script)
bridgeAddress :: Address
bridgeAddress = Ledger.scriptAddress bridgeValidator

bridgeValidator :: Validator
bridgeValidator = Scripts.validatorScript bridgeInstance

bridgeInstance :: Scripts.TypedValidator Bridge
bridgeInstance = Scripts.mkTypedValidator @Bridge
    $$(PlutusTx.compile [|| validateUpdateVerifiers ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @VerifierInfo @VerifierInfo

verifiers :: AsContractError e => Promise () BridgeSchema e ()
verifiers = endpoint @"verifiers" @VerifierInfo $ \vi -> do
    logInfo @Haskell.String $ "Setting verifiers and threshold " -- <> (C.unpack $ Builtins.fromBuiltin vi)
    let tx         = Constraints.mustPayToTheScript vi mempty
    void $ submitTxConstraints bridgeInstance tx

validateUpdateVerifiers :: VerifierInfo -> VerifierInfo -> ScriptContext -> Bool
validateUpdateVerifiers _ _ _ = True
-- validateUpdateVerifiers (VerifierInfo hashes _ ) cs _
--  | null hashes = True
--  | otherwise = elem cs hashes

bridge :: AsContractError e => Contract () BridgeSchema e ()
bridge = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [verifiers] >> bridge

{-
lockTrace :: Wallet -> Haskell.String -> EmulatorTrace ()
lockTrace wallet secretWord = do
    hdl <- Trace.activateContractWallet wallet (lock @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"lock" hdl (LockParams "ckb1qyqrdsefa43s6m882pcj53m4gdnj4k440axqdt9rtd" (Ada.adaValueOf 10))
    void $ Trace.waitNSlots 1
-}
-- mkSchemaDefinitions ''BridgeSchema
{-
correctGuessTrace :: EmulatorTrace ()
correctGuessTrace = do
  let w1 = X.knownWallet 1
      w2 = X.knownWallet 2
      secret = "secret"

  h1 <- Trace.activateContractWallet w1 (lock @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock" h1 (LockParams "ckb1qyqrdsefa43s6m882pcj53m4gdnj4k440axqdt9rtd" (Ada.adaValueOf 10))
-}
