module Language.Piet.Internal.LLVM
  ( withLinkedModule
  , resolver
  , withHostDynamicJITTargetMachine
  ) where

import qualified Language.Piet.Runtime as P
import qualified LLVM.AST as AST
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import LLVM.Context
import LLVM.Linking
import LLVM.Module
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import qualified LLVM.Relocation as Reloc
import LLVM.Target

withLinkedModule :: AST.Module -> (Module -> IO a) -> IO a
withLinkedModule ast f =
  withContext $ \ctx ->
    withModuleFromLLVMAssembly ctx P.runtimeAssembly $ \runtimeMod ->
      withModuleFromAST ctx ast $ \astMod -> do
        linkModules astMod runtimeMod
        let linkedModule = astMod
        f linkedModule

resolver :: IRCompileLayer l -> SymbolResolver
resolver cl = SymbolResolver dylibResolver' externalResolver' where
  dylibResolver' sym = findSymbol cl sym True
  externalResolver' sym = do
    addr <- getSymbolAddressInProcess sym
    return $ return $ JITSymbol addr defaultJITSymbolFlags

withHostDynamicJITTargetMachine :: (TargetMachine -> IO a) -> IO a
withHostDynamicJITTargetMachine f = do
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.Default CodeModel.JITDefault CodeGenOpt.Default f
