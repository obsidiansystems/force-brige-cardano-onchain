{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ImportQualifiedPost   #-}

module ForceBridge.Trace where

import Data.Maybe
import Control.Monad.Freer
import Control.Monad.Freer.Extras.Log (logInfo)
import Ledger hiding (singleton)
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet qualified as CW
import Wallet.Emulator.Wallet
import qualified Data.Text as T
-- import Plutus.Contract (logInfo)
import Plutus.Contract.StateMachine (ThreadToken)
import qualified Plutus.Contract.StateMachine as SM
import Plutus.Contract.Test.ContractModel
import Plutus.Trace.Emulator as Trace
import Test.QuickCheck as QC hiding (checkCoverage, (.&&.))
import Plutus.Contract.Test
import ForceBridge (BridgeDatum(..), BridgeSchema, mapSMError)
import ForceBridge.Orphans ()
import qualified ForceBridge as FB

import Control.Lens hiding (elements)

-- type VerifierList = ([Wallet], Integer)
type VerifierList = ([CW.MockWallet], Integer)

instance Eq CW.MockWallet where
  _ == _ = False
  -- show = "lksdjf"

data BridgeModel =
  BridgeModel { _verifiers :: Maybe VerifierList
              , _bridgeValue :: Integer
              }
  deriving (Show)

makeLenses ''BridgeModel

deriving instance Eq (ContractInstanceKey BridgeModel w schema err params)
deriving instance Ord (ContractInstanceKey BridgeModel w schema err params)
deriving instance Show (ContractInstanceKey BridgeModel w schema err params)

-- TODO(skylar): Do we need a custom error?
-- data BridgeError = BridgeError
instance ContractModel BridgeModel where

  data ContractInstanceKey BridgeModel w schema err params where
    WalletKey :: Wallet -> ContractInstanceKey BridgeModel () BridgeSchema T.Text ()

  data Action BridgeModel = Init Wallet VerifierList
                          | Lock Wallet Integer
                          | Unlock Wallet Integer
                          | Update Wallet VerifierList
       deriving (Eq, Show)

  initialInstances = (`StartContract` ()) . WalletKey <$> wallets

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} _ = FB.contract

  initialState = BridgeModel
    { _bridgeValue = 0
    , _verifiers = Nothing
    }

  arbitraryAction _ = oneof $
                      [Init <$> genWallet <*> genVerifiers] ++
                      [Lock <$> genWallet <*> genValue] ++
                      [Unlock <$> genWallet <*> genValue] ++
                      [Update <$> genWallet <*> genVerifiers]

  perform handle _ s = \case
    Init w vl -> do
      let
        bd = mkBridgeDatum vl
      logInfo $ show vl
      Trace.callEndpoint @"init" (handle $ WalletKey w) $ FB.InitParams Nothing bd
      delay 2

    Lock w ada -> do
      let
        v = Ada.lovelaceValueOf ada
      Trace.callEndpoint @"lock" (handle $ WalletKey w) $ FB.LockParams Nothing v
      delay 1

    Unlock w ada -> do
      let
        vs = fromJust (s ^. contractState . verifiers)
        v = Ada.lovelaceValueOf ada
        addr = mockWalletPaymentPubKeyHash w
      -- logInfo @String ")what the hell"
      -- Trace.logInfo "Heheh"
      Trace.setSigningProcess w $ signWithVerifiers vs
      Trace.callEndpoint @"unlock" (handle $ WalletKey w) $ FB.UnlockParams Nothing v addr
      -- Trace.setSigningProcess w $ mockWalletDefaultSign w
      delay 1

    Update w vl -> do
      let
        vs = fromJust (s ^. contractState . verifiers)
        bd = mkBridgeDatum vl
      Trace.setSigningProcess w $ signWithVerifiers vs
      Trace.callEndpoint @"update" (handle $ WalletKey w) $ FB.UpdateParams Nothing bd
      -- Trace.setSigningProcess w $ mockWalletDefaultSign w
      delay 1

  nextState = \case
    Init _ bd -> do
      verifiers .= Just bd
      wait 2

    Lock w a -> do
      bridgeValue %= (+a)
      withdraw w $ Ada.lovelaceValueOf a
      wait 1

    Unlock w a -> do
      bridgeValue %= (subtract a)
      deposit w $ Ada.lovelaceValueOf a
      wait 1

    Update _ nd -> do
      verifiers .= Just nd
      wait 1

  precondition s = \case
    Init _ _ -> isNothing vs
    _ -> isJust vs
    where
      vs = s ^. contractState . verifiers

  monitoring _ _ = id

-- signWithVerifiers :: BridgeDatum -> SigningProcess
-- signPrivateKeys [CW.paymentPrivateKey (CW.knownMockWallet 1), CW.paymentPrivateKey (CW.knownMockWallet 2)]

wallets :: [Wallet]
wallets = [w1,w2, w3]

verifierWallets :: [Wallet]
verifierWallets = [w4, w5, w6, w7, w8, w9, w10]

genWallet :: Gen Wallet
genWallet = elements wallets

genVerifierWallet :: Gen Wallet
genVerifierWallet = elements verifierWallets

genVerifiers :: Gen VerifierList
genVerifiers = do
  pure testerino
    -- (verifierWallets, fromIntegral $ length verifierWallets - 1)

{-
  n <- choose (1, length verifierWallets)
  let vs = take n verifierWallets -- vectorOf n verifierWallets
  minSig <- chooseInt (1, length vs)
  pure $ (vs, fromIntegral minSig)
-}

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary

mkBridgeDatum :: VerifierList -> BridgeDatum
mkBridgeDatum vl = BridgeDatum pubKeys (snd vl)
  where
    pubKeys = mockWalletPaymentPubKeyHash . knownWallet <$> [1..3]
      -- fmap (unPaymentPubKeyHash . mockWalletPaymentPubKeyHash). fst $ vl

signWithVerifiers :: VerifierList -> SigningProcess
signWithVerifiers (vs,_) =
  signPrivateKeys [CW.paymentPrivateKey (CW.knownMockWallet 1), CW.paymentPrivateKey (CW.knownMockWallet 2), CW.paymentPrivateKey (CW.knownMockWallet 3)]
  -- signPrivateKeys $ fmap mockWalletPaymentPrivateKey vs

mockWalletDefaultSign :: Wallet -> SigningProcess
mockWalletDefaultSign w =
  defaultSigningProcess
    $ fromMaybe (error $ "mockWalletPaymentPrivateKey: Wallet "
                          <> show w
                          <> " is not a mock wallet")
    $ walletToMockWallet w

mockWalletPaymentPrivateKey :: Wallet -> PaymentPrivateKey
mockWalletPaymentPrivateKey w =
  CW.paymentPrivateKey
    $ fromMaybe (error $ "mockWalletPaymentPrivateKey: Wallet "
                      <> show w
                      <> " is not a mock wallet")
    $ walletToMockWallet w

-- | The main property. 'propRunActions_' checks that balances match the model after each test.
prop_Bridge :: Actions BridgeModel -> Property
prop_Bridge = propRunActions_

testerino :: VerifierList
testerino = (CW.knownMockWallet <$> [1..5], 3)

{-
succeedingTrace :: EmulatorTrace ()
succeedingTrace = do
    hdl <- Trace.activateContractWallet w1 theContract
    Trace.callEndpoint @"lock" hdl (multiSig, Ada.lovelaceValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.setSigningProcess w1 (signPrivateKeys [CW.paymentPrivateKey (CW.knownMockWallet 1), CW.paymentPrivateKey (CW.knownMockWallet 2), CW.paymentPrivateKey (CW.knownMockWallet 3)])
    Trace.callEndpoint @"unlock" hdl (multiSig, fmap mockWalletPaymentPubKeyHash [w1, w2, w3])
    void $ Trace.waitNSlots 1

theContract :: Contract () MultiSigSchema ContractError ()
theContract = MS.contract

-- a 'MultiSig' contract that requires three out of five signatures
multiSig :: MultiSig
multiSig = MultiSig
        { signatories = mockWalletPaymentPubKeyHash . knownWallet <$> [1..5]
        , minNumSignatures = 3
-}
