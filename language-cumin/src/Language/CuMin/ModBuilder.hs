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
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Result

buildModuleFromFile :: MonadIO m => FilePath -> m (Either Doc Module)
buildModuleFromFile cuminFile = parseCuMinFileEx cuminFile >>= \case
    Failure msg -> return $ Left msg
    Success decls -> return $ buildModuleFromDecls (takeBaseName cuminFile) decls

buildModuleFromDecls :: String -> [Decl] -> Either Doc Module
buildModuleFromDecls name decls =
  let
    adts = [adt | DData adt <- decls]
    bnds = [bnd | DTop bnd <- decls]
  in buildModule name adts bnds
