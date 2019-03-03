{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

-- | Functions to generate assembly code from 'SyntaxGraph'.
module Language.Piet.AssemblyGenerator
  ( generateAssembly
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as B
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe
import GHC.Exts
import Language.Piet.Internal.DPCC
import qualified Language.Piet.Runtime as P
import Language.Piet.Syntax
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder as IR

-- | Generate an AST from a 'SyntaxGraph'.
generateAssembly :: SyntaxGraph -> AST.Module
generateAssembly syntax = IR.buildModule "" $ do
  P.declareFunctions
  _ <- IR.function "main" [] T.i32 $ \_ -> mainFunction $ getSyntaxGraph syntax
  return ()

mainFunction :: (MonadFix m, IR.MonadModuleBuilder m) => IntMap Block -> IR.IRBuilderT m ()
mainFunction graph = mdo
  _ <- IR.block `IR.named` "entry"
  dpccPtr <- IR.alloca T.i32 Nothing 4 `IR.named` "dpcc_ptr"
  IR.store dpccPtr 4 $ int32 0

  IR.br $ fromMaybe exitLabel $ labelTable IM.!? 0

  let blocks = IM.toAscList graph
  labelTable <- IM.fromList <$> mapM (\(n, block) -> (n,) <$> step exitLabel labelTable dpccPtr n block) blocks

  exitLabel <- IR.block `IR.named` "exit"
  P.resetStack
  IR.ret $ int32 0

step :: (MonadFix m, IR.MonadIRBuilder m) => AST.Name -> IntMap AST.Name -> AST.Operand -> Int -> Block -> m AST.Name
step exitLabel labelTable dpccPtr index block = mdo
  let stepLabelName = stringToShort $ "block_" ++ show index
  stepLabel <- IR.block `IR.named` stepLabelName

  dpcc <- IR.load dpccPtr 4 `IR.named` stringToShort ("dpcc_" ++ show index)
  let dpccToLabelList = first dpccToInteger <$> tableToList switchLabelTable
  IR.switch dpcc exitLabel $ first (C.Int 32) <$> sortWith fst dpccToLabelList

  let blockTable = M.toAscList $ nextBlockTable block
  let backwardDPCCTable = dpccsToBackwardDPCCTable $ fst <$> blockTable
  switchLabelTable <- forM blockTable $ \(nextDPCC, (command, nextBlockIndex)) -> do
    let branchLabelName = stringToShort $ "jump_" ++ show index ++ "_" ++ show nextBlockIndex ++ "_" ++ showDPCC nextDPCC
    branchLabel <- IR.block `IR.named` branchLabelName
    let currentDPCCs = backwardDPCCTable M.! nextDPCC
    when (currentDPCCs /= [nextDPCC]) $ IR.store dpccPtr 4 $ int32 $ dpccToInteger nextDPCC
    commandToLLVMInstruction command dpccPtr
    let nextLabel = labelTable IM.! nextBlockIndex  -- unsafe
    IR.br nextLabel
    return (currentDPCCs, branchLabel)

  return stepLabel

dpccToInteger :: (DirectionPointer, CodelChooser) -> Integer
dpccToInteger (dp, cc) = toInteger $ fromEnum dp * 2 + fromEnum cc

tableToList :: [([a], b)] -> [(a, b)]
tableToList table = do
  (as, b) <- table
  a <- as
  return (a, b)

commandToLLVMInstruction :: IR.MonadIRBuilder m => Command -> AST.Operand -> m ()
commandToLLVMInstruction NoOperation _       = return ()
commandToLLVMInstruction (Push n)    _       = P.push n
commandToLLVMInstruction Pop         _       = P.pop
commandToLLVMInstruction Add         _       = P.add
commandToLLVMInstruction Subtract    _       = P.subtract
commandToLLVMInstruction Multiply    _       = P.multiply
commandToLLVMInstruction Divide      _       = P.divide
commandToLLVMInstruction Mod         _       = P.mod
commandToLLVMInstruction Not         _       = P.not
commandToLLVMInstruction Greater     _       = P.greater
commandToLLVMInstruction Pointer     dpccPtr = P.pointer dpccPtr
commandToLLVMInstruction Switch      dpccPtr = P.switch dpccPtr
commandToLLVMInstruction Duplicate   _       = P.duplicate
commandToLLVMInstruction Roll        _       = P.roll
commandToLLVMInstruction InNumber    _       = P.inNumber
commandToLLVMInstruction InChar      _       = P.inChar
commandToLLVMInstruction OutNumber   _       = P.outNumber
commandToLLVMInstruction OutChar     _       = P.outChar

int32 :: Integer -> AST.Operand
int32 = AST.ConstantOperand . C.Int 32

showDPCC :: (DirectionPointer, CodelChooser) -> String
showDPCC (dp, cc) = [charDP dp, charCC cc] where
  charDP DPRight = 'r'
  charDP DPDown  = 'd'
  charDP DPLeft  = 'l'
  charDP DPUp    = 'u'
  charCC CCLeft  = 'l'
  charCC CCRight = 'r'

stringToShort :: String -> ShortByteString
stringToShort = B.toShort . BC.pack
