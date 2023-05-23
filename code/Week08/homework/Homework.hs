{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework
  ( stakeValidator',
    saveStakeValidator',
  )
where



import Plutus.V2.Ledger.Api
  ( Address,
    BuiltinData,
    PubKeyHash,
    ScriptContext,
    StakeValidator,
    getValue,
    mkStakeValidatorScript,
    scriptContextTxInfo,
    txInfoOutputs,
    txInfoSignatories,
    txOutValue,
  )
import Plutus.V2.Ledger.Tx (txOutAddress)
import qualified PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude (Bool (..), divide, ($), (&&), (*), (+), (.), (==), (>=))
import qualified PlutusTx.Prelude as PlutusTx
import Utilities (wrapStakeValidator)
import Prelude (IO, String, elem, filter, foldr, undefined)

-- | A staking validator with two parameters, a pubkey hash and an address. The validator
--   should work as follows:
--   1.) The given pubkey hash needs to sign all transactions involving this validator.
--   2.) The given address needs to receive at least half of all withdrawn rewards.
mkStakeValidator' :: PubKeyHash -> Address -> () -> ScriptContext -> PlutusTx.Bool
mkStakeValidator' _pkh _addr () ctx =
  let info = scriptContextTxInfo ctx
      signer = txInfoSignatories info
      outputs = PlutusTx.map txOutAddress $ txInfoOutputs info
      rewards = filter (\output -> txOutValue output == 1) (txInfoOutputs info)
      rewardAmount = PlutusTx.sum $ PlutusTx.map (getValue . txOutValue) rewards
      halfRewardAmount = rewardAmount `divide` 2
   in signer == [_pkh] && _addr `PlutusTx.elem` outputs && rewardAmount >= halfRewardAmount

{-# INLINEABLE mkWrappedStakeValidator' #-}
mkWrappedStakeValidator' :: PubKeyHash -> Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator' pkh = wrapStakeValidator . mkStakeValidator' pkh

stakeValidator' :: PubKeyHash -> Address -> StakeValidator
stakeValidator' pkh addr =
  mkStakeValidatorScript $
    $$(PlutusTx.compile [||mkWrappedStakeValidator'||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh
      `PlutusTx.applyCode` PlutusTx.liftCode addr

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveStakeValidator' :: String -> String -> IO ()
saveStakeValidator' _pkh _bech32 = undefined
