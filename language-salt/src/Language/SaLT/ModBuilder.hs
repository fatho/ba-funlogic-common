{-# LANGUAGE LambdaCase #-}
module Language.SaLT.ModBuilder where

import           Control.Applicative
import           Control.Monad.Writer
import           Language.Haskell.TH.Quote
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Result

import           FunLogic.Core.ModBuilder
import           FunLogic.Core.TH
import           Language.SaLT.AST
import           Language.SaLT.Parser
import           Language.SaLT.Prelude
import           Language.SaLT.TH

buildSaltModuleFromFile :: MonadIO m => String -> m (Either Doc Module)
buildSaltModuleFromFile saltFile = parseSaltFileEx saltFile >>= \case
    Failure msg -> return $ Left msg
    Success decls -> return $ buildSaltModuleFromDecls decls

buildSaltModuleFromDecls :: [Decl] -> Either Doc Module
buildSaltModuleFromDecls decls =
  let
    adts = preludeADTs ++ [adt | DData adt <- decls]
    bnds = preludeBindings ++ [bnd | DTop bnd <- decls]
  in buildModule "Main" adts bnds

-- This cannot go into TH.hs because of cyclic module dependencies.
saltModule :: QuasiQuoter
saltModule = makeQQ dataToExp $ \str ->
  (buildSaltModuleFromDecls <$> runParserQ program "<quasi-quoted module>" str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show msg ++"`\n"
    check (Right m) = return m
