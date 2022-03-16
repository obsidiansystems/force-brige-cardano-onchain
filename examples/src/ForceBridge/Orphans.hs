{-# OPTIONS_GHC -Wno-orphans #-}

module ForceBridge.Orphans where

import Test.QuickCheck
import Ledger hiding (singleton)
import Data.ByteString (ByteString)
import Plutus.Contract.StateMachine (ThreadToken)

import Plutus.Contract.Test ()
import Test.QuickCheck.Arbitrary.Generic ( Arbitrary
                                         , arbitrary
                                         , genericArbitrary
                                         , genericShrink
                                         , shrink
                                         )

import qualified PlutusTx.Prelude as PlutusTx

instance Arbitrary Ledger.CurrencySymbol where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ThreadToken where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PlutusTx.BuiltinByteString where
    arbitrary = PlutusTx.toBuiltin <$> (arbitrary :: Gen ByteString)

instance Arbitrary TxOutRef where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxId where
    arbitrary = genericArbitrary
    shrink = genericShrink
