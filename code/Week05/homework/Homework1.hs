{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash, MintingPolicy,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, to, mkMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool, traceIfFalse, ($), (&&), (.))
import           Utilities            (wrapPolicy)



{-# INLINABLE mkDeadlinePolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkDeadlinePolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkDeadlinePolicy _pkh _deadline () _ctx =  signedByBeneficiary  && deadlineReached

    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx
        
        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info $  _pkh

        deadlineReached :: Bool
        deadlineReached = contains (to $  _deadline) $ txInfoValidRange info


{-# INLINABLE mkWrappedDeadlinePolicy #-}
mkWrappedDeadlinePolicy :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> ()
mkWrappedDeadlinePolicy pkh deadline = wrapPolicy $ mkDeadlinePolicy pkh deadline

deadlinePolicy :: PubKeyHash -> POSIXTime -> MintingPolicy
deadlinePolicy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedDeadlinePolicy ||])
