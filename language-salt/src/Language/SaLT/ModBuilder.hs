{-# LANGUAGE LambdaCase #-}
module Language.SaLT.ModBuilder
  ( buildModuleFromFile
  , buildModuleFromDecls
  , saltModule
  , module FunLogic.Core.ModBuilder
  ) where

import           Control.Applicative
import           Control.Monad.Writer
import           FunLogic.Core.ModBuilder
import           Language.Haskell.TH.Quote
import           Language.SaLT.AST
import           Language.SaLT.Parser
import           Language.SaLT.TH
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
    adts = [adt | DData adt <- decls]
    bnds = [bnd | DTop bnd <- decls]
  in buildModule name adts bnds

-- This cannot go into TH.hs because of cyclic module dependencies.
saltModule :: String -> QuasiQuoter
saltModule name = makeQQ dataToExp $ \str ->
  (buildModuleFromDecls name <$> runParserQ program "<quasi-quoted module>" str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show msg ++"`\n"
    check (Right m) = return m
