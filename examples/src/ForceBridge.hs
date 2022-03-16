{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module ForceBridge where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Text           as T
import           Control.Monad       hiding (fmap)
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract              as Contract hiding (when)
import           Plutus.Contract.StateMachine ( State(..)
                                              , StateMachine(..)
                                              , StateMachineClient(..)
                                              , StateMachineInstance(..)
                                              , SMContractError
                                              , ThreadToken
                                              , mkStateMachineClient
                                              , runInitialise
                                              , getOnChainState
                                              , runStep
                                              )
import PlutusTx.Builtins.Class (stringToBuiltinString)
import qualified Plutus.Contract.StateMachine as SM
import           PlutusTx            (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude    hiding (Semigroup(..), unless, check, init)
import           Ledger              hiding (singleton)
import qualified Ledger.Contexts     as Contexts
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import qualified Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import           Ledger.Ada          as Ada
import           Ledger.Value
import           Prelude             (IO, Semigroup (..), String)
import qualified Prelude             as Haskell

-- | Bridge is parameterized by the NFT that will be used as the thread token
type Bridge = Maybe ThreadToken

data BridgeDatum = BridgeDatum
  { bridgeStateVerifiers :: [PaymentPubKeyHash]
  , bridgeStateThreshold :: Haskell.Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- PlutusTx.unstableMakeIsData ''BridgeDatum

-- TODO(skylar): Do these need unified types
data BridgeRedeemer
  = Lock Value
  | Unlock Value PaymentPubKeyHash
  | Update BridgeDatum
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- PlutusTx.unstableMakeIsData ''BridgeRedeemer

type BridgeStateMachine = StateMachine BridgeDatum BridgeRedeemer
instance Scripts.ValidatorTypes BridgeStateMachine where
  type instance DatumType BridgeStateMachine = BridgeDatum
  type instance RedeemerType BridgeStateMachine = BridgeRedeemer

bridgeStateMachine :: Bridge -> StateMachine BridgeDatum BridgeRedeemer
bridgeStateMachine bridge = StateMachine
  { smTransition = transition
  , smFinal = final
  , smCheck = check
  , smThreadToken = bridge
  }

{-# INLINABLE transition #-}
transition :: SM.State BridgeDatum -> BridgeRedeemer -> Maybe (TxConstraints Void Void, SM.State BridgeDatum)
transition s r = case (stateValue s, stateData s, r) of
  (v, d, Lock lockVal) -> Just (mempty, State d (v <> lockVal))
  (v, d, Unlock unlockVal toAddr) -> Just (Constraints.mustPayToPubKey toAddr unlockVal, State d (v - unlockVal))
  (v, _, Update d') -> Just (mempty, State d' v)

{-# INLINABLE check #-}
check :: BridgeDatum -> BridgeRedeemer -> ScriptContext -> Bool
check (BridgeDatum signatories minSignatures) r ctx =
  traceIfFalse "OH NO" (doesntNeedApproval r || ((> 1) . length . txInfoSignatories . scriptContextTxInfo $ ctx)) &&
  traceIfFalse "Not enough signatures" (doesntNeedApproval r || enoughSignatures)
  where
    presentSignatures = length (filter (Contexts.txSignedBy (scriptContextTxInfo ctx) . unPaymentPubKeyHash) signatories)
    enoughSignatures = presentSignatures >= minSignatures

{-# INLINABLE doesntNeedApproval #-}
doesntNeedApproval :: BridgeRedeemer -> Bool
doesntNeedApproval (Lock _) = True
doesntNeedApproval _ = False

{-# INLINABLE final #-}
final :: BridgeDatum -> Bool
final _ = False

{-# INLINABLE mkBridgeValidator #-}
mkBridgeValidator :: Bridge -> BridgeDatum -> BridgeRedeemer -> ScriptContext -> Bool
mkBridgeValidator bridge = SM.mkValidator $ bridgeStateMachine bridge

bridgeInstance :: Bridge -> Scripts.TypedValidator BridgeStateMachine
bridgeInstance = Scripts.mkTypedValidatorParam @BridgeStateMachine
  ($$(PlutusTx.compile [|| mkBridgeValidator ||]))
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BridgeDatum @BridgeRedeemer

bridgeClient :: Bridge -> StateMachineClient BridgeDatum BridgeRedeemer
bridgeClient bridge = mkStateMachineClient $ StateMachineInstance (bridgeStateMachine bridge) (bridgeInstance bridge)

type BridgeSchema =
   --BlockchainActions
   Endpoint "init" InitParams
   .\/ Endpoint "lock" LockParams
   .\/ Endpoint "unlock" UnlockParams
   .\/ Endpoint "update" UpdateParams

data InitParams = InitParams
  { ipBridge :: Bridge
  , ipBridgeSettings :: BridgeDatum
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

init :: Promise w BridgeSchema T.Text ()
init = endpoint @"init" @InitParams $ \ip -> do
  let
    bridge = ipBridge ip
    client = bridgeClient bridge
  void $ mapSMError $ runInitialise client (ipBridgeSettings ip) mempty
  logInfo @String "Built bridge"
  return ()

data LockParams = LockParams
  { lpBridge :: Bridge
  , lpValue :: Value
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

lock :: Promise w BridgeSchema T.Text ()
lock = endpoint @"lock" @LockParams $ \lp -> do
  let
    bridge = lpBridge lp
    client = bridgeClient bridge
    v = lpValue lp
  void $ mapSMError $ runStep client $ Lock v
  logInfo @String "Locked assets"
  return ()

data UnlockParams = UnlockParams
  { upBridge :: Bridge
  , upValue :: Value
  , upTo :: PaymentPubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

unlock :: Promise w BridgeSchema T.Text ()
unlock = endpoint @"unlock" @UnlockParams $ \up -> do
  let
    bridge = upBridge up
    client = bridgeClient bridge
    v = upValue up
    toAddr = upTo up
  void $ mapSMError $ runStep client $ Unlock v toAddr
  logInfo @String "Unlocked assets"
  return ()

data UpdateParams = UpdateParams
  { updateBridge :: Bridge
  , updateSettings :: BridgeDatum
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

update :: Promise w BridgeSchema T.Text ()
update = endpoint @"update" @UpdateParams $ \up -> do
  let
    bridge = updateBridge up
    client = bridgeClient bridge
    settings = updateSettings up
  void $ mapSMError $ runStep client $ Update settings
  logInfo @String "Updated verifiers"
  return ()

contract :: Contract () BridgeSchema T.Text ()
contract = selectList [init, lock, unlock, update] >> contract

mapSMError :: Contract w s SMContractError a -> Contract w s Text a
mapSMError = mapError (T.pack . Haskell.show @SM.SMContractError)

PlutusTx.unstableMakeIsData ''BridgeDatum
PlutusTx.unstableMakeIsData ''BridgeRedeemer
PlutusTx.unstableMakeIsData ''UpdateParams
