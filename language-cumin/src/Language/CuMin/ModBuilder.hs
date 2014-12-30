{-# LANGUAGE LambdaCase #-}
module Language.CuMin.ModBuilder
  ( buildModuleFromFile
  , buildModuleFromDecls
  , module FunLogic.Core.ModBuilder
  ) where

import           Control.Applicative
import           Control.Monad.Writer
import           FunLogic.Core.ModBuilder
import           FunLogic.Core.TH
import           Language.CuMin.AST
import           Language.CuMin.Parser
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
