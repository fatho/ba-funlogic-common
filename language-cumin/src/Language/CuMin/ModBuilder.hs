{-# LANGUAGE LambdaCase #-}
module Language.CuMin.ModBuilder
  ( buildModuleFromFile
  , buildModuleFromDecls
  , cuminModule
  , module FunLogic.Core.ModBuilder
  ) where

import           Control.Applicative
import           Control.Monad.Writer
import           FunLogic.Core.ModBuilder
import           FunLogic.Core.TH
import           Language.CuMin.AST
import           Language.CuMin.Parser
import           Language.CuMin.TH
import           Language.Haskell.TH.Quote
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

-- This cannot go into TH.hs because of cyclic module dependencies.
cuminModule :: String -> QuasiQuoter
cuminModule name = makeQQ dataToExp $ \str ->
  (buildModuleFromDecls name <$> runParserQ program "<quasi-quoted module>" str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show msg ++"`\n"
    check (Right m) = return m
