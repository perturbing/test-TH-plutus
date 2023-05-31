{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}


module Validator where

import           PlutusTx.Prelude             (Bool(..), BuiltinByteString, (==), Integer)
import           PlutusTx                     (BuiltinData, CompiledCode)
import           Utilities                    (writeCodeToFile, wrapPolicy)
import           Prelude                      (IO)
import           PlutusLedgerApi.V2           (ScriptContext(..), toBuiltin )
import           PlutusTx.TH                  as TH

import           Language.Haskell.TH.Syntax   (lift)
import           Test                         (bytesFromHex, multiplyInt)


{-# INLINABLE policyWithLiftInt #-}
policyWithLiftInt :: Integer -> ScriptContext -> Bool
policyWithLiftInt red _ctx = red == $(lift (multiplyInt 5))

{-# INLINABLE mkWrappedLiftInt #-}
mkWrappedLiftInt :: BuiltinData -> BuiltinData -> ()
mkWrappedLiftInt = wrapPolicy policyWithLiftInt

policyWithLiftIntCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
policyWithLiftIntCode = $$(compile [|| mkWrappedLiftInt ||])

savePolicyWithLiftInt :: IO ()
savePolicyWithLiftInt = writeCodeToFile "../assets/policy-with-liftInt.plutus" policyWithLiftIntCode

{-# INLINABLE policyWithLiftBS #-}
policyWithLiftBS :: BuiltinByteString -> ScriptContext -> Bool
policyWithLiftBS red _ctx = red == toBuiltin $(lift (bytesFromHex "ff"))

{-# INLINABLE mkWrappedLiftBS #-}
mkWrappedLiftBS :: BuiltinData -> BuiltinData -> ()
mkWrappedLiftBS = wrapPolicy policyWithLiftBS

policyWithLiftBSCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
policyWithLiftBSCode = $$(compile [|| mkWrappedLiftBS ||])

savePolicyWithLiftBS :: IO ()
savePolicyWithLiftBS = writeCodeToFile "../assets/policy-with-liftiBS.plutus" policyWithLiftBSCode