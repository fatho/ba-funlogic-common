{-# LANGUAGE LambdaCase #-}
module Language.SaLT.ModBuilder
  ( buildModuleFromFile
  , buildModuleFromDecls
  , module FunLogic.Core.ModBuilder
  ) where

import           Control.Monad.Writer
import           FunLogic.Core.ModBuilder
import           Language.SaLT.AST
import           Language.SaLT.Parser
import           System.FilePath              (takeBaseName)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Result

buildModuleFromFile :: MonadIO m => String -> m (Either Doc Module)
buildModuleFromFile saltFile = parseSaltFileEx saltFile >>= \case
    Failure msg -> return $ Left msg
    Success decls -> return $ buildModuleFromDecls (takeBaseName saltFile) decls

buildModuleFromDecls :: String -> [Decl] -> Either Doc Module
buildModuleFromDecls name decls =
  let
    adts = [adtd | DData adtd <- decls]
    bnds = [bnd | DTop bnd <- decls]
  in buildModule name adts bnds
