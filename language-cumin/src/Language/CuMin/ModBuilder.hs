{-# LANGUAGE LambdaCase #-}
module Language.CuMin.ModBuilder
  ( buildModuleFromFile
  , buildModuleFromDecls
  , loadAndCheckCuMin
  , module FunLogic.Core.ModBuilder
  ) where

import           Control.Monad.Writer
import qualified Data.Map as M
import           FunLogic.Core.ModBuilder
import           Language.CuMin.AST
import           Language.CuMin.Parser
import           Language.CuMin.TypeChecker
import           System.FilePath              (takeBaseName)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta.Result

-- | Builds a CuMin module from a source file.
buildModuleFromFile :: MonadIO m => FilePath -> m (Either PP.Doc Module)
buildModuleFromFile cuminFile = parseCuMinFileEx cuminFile >>= \case
    Failure msg -> return $ Left msg
    Success decls -> return $ buildModuleFromDecls (takeBaseName cuminFile) decls

-- | Builds a CuMin module from a list of declarations.
buildModuleFromDecls :: ModName -> [Decl] -> Either PP.Doc Module
buildModuleFromDecls name decls =
  let
    adts = [adt | DData adt <- decls]
    bnds = [bnd | DTop bnd <- decls]
  in buildModule name adts bnds

-- | Loads a CuMin file, imports the prelude and passes it to the type checker.
-- The first module returned is the raw module loaded from the file, the second returned module is the module
-- from the file merged with the supplied prelude module.
loadAndCheckCuMin :: Module -> FilePath -> IO (Either PP.Doc (Module, Module))
loadAndCheckCuMin prelude filePath = buildModuleFromFile filePath >>= \case
  Left e -> return $ Left e
  Right modul -> case modul `importUnqualified` prelude of
    Left (dupData, dupBinds) -> return $ Left $ PP.text "Module definitions are overlapping prelude:"
        PP.<+> PP.hsep (map PP.text $ M.keys dupData) PP.<+> PP.hsep (map PP.text $ M.keys dupBinds)
    Right modul' -> case evalTC' $ checkModule modul' of
      Left e -> return $ Left $ PP.pretty e
      Right _ -> return $ Right (modul, modul')
