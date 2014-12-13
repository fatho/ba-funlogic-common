{-# LANGUAGE LambdaCase #-}
module Language.CuMin.ModBuilder
  ( buildModuleFromFile
  , buildModuleFromDecls
  , cuminModule
  , module FunLogic.Core.ModBuilder
  ) where

import           Control.Applicative
import           Control.Monad.Writer
import           Language.Haskell.TH.Quote
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Result

import           FunLogic.Core.ModBuilder
import           FunLogic.Core.TH
import           Language.CuMin.AST
import           Language.CuMin.Parser
import           Language.CuMin.Prelude
import           Language.CuMin.TH

buildModuleFromFile :: MonadIO m => String -> m (Either Doc Module)
buildModuleFromFile cuminFile = parseCuMinFileEx cuminFile >>= \case
    Failure msg -> return $ Left msg
    Success decls -> return $ buildModuleFromDecls decls

buildModuleFromDecls :: [Decl] -> Either Doc Module
buildModuleFromDecls decls =
  let
    adts = preludeADTs ++ [adt | DData adt <- decls]
    bnds = preludeBindings ++ [bnd | DTop bnd <- decls]
  in buildModule "Main" adts bnds

-- This cannot go into TH.hs because of cyclic module dependencies.
cuminModule :: QuasiQuoter
cuminModule = makeQQ dataToExp $ \str ->
  (buildModuleFromDecls <$> runParserQ program "<quasi-quoted module>" str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show msg ++"`\n"
    check (Right m) = return m
