{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, MintingPolicy,
                                       ScriptContext, TokenName, TxOutRef, TxInfo,
                                       mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude     (Bool (False), ($), (.))
import           Utilities            (wrapPolicy)

{-# INLINABLE mkEmptyNFTPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                       traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

{-# INLINABLE mkWrappedEmptyNFTPolicy #-}
mkWrappedEmptyNFTPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedEmptyNFTPolicy = wrapPolicy . mkEmptyNFTPolicy

nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $ $$(PlutusTx.compile [|| mkWrappedEmptyNFTPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref

        `PlutusTx.applyCode` PlutusTx.liftCode pkh
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
