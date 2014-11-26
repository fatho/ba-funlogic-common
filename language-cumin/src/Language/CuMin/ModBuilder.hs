{-# LANGUAGE LambdaCase #-}
module Language.CuMin.ModBuilder where

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

buildCuMinModuleFromFile :: MonadIO m => String -> m (Either Doc Module)
buildCuMinModuleFromFile cuminFile = parseCuMinFileEx cuminFile >>= \case
    Failure msg -> return $ Left msg
    Success decls -> return $ buildCuMinModuleFromDecls decls

buildCuMinModuleFromDecls :: [Decl] -> Either Doc Module
buildCuMinModuleFromDecls decls =
  let
    adts = preludeADTs ++ [adt | DData adt <- decls]
    bnds = preludeBindings ++ [bnd | DTop bnd <- decls]
  in buildModule "Main" adts bnds

-- This cannot go into TH.hs because of cyclic module dependencies.
--
-- This does not work yet. Error message from GHC:
-- "Illegal data constructor name: ‘fromList’ When splicing a TH expression: ..."
-- Is something wrong with the Data instance for Map?
-- TODO: FIX this
cuminModule :: QuasiQuoter
cuminModule = makeQQ $ \str ->
  (buildCuMinModuleFromDecls <$> runParserQ program "<quasi-quoted module>" str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show msg ++"`\n"
    check (Right m) = return m
