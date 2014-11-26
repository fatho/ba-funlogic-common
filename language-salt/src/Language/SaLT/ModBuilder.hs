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
--
-- This does not work yet. Error message from GHC:
-- "Illegal data constructor name: ‘fromList’ When splicing a TH expression:
-- FunLogic.Core.AST.CoreModule ((GHC.Types.:) 'M' ((GHC.Types.:) 'a' ((GHC.Types.:) 'i' ((GHC.Types.:) 'n' GHC.Types.[])))) (Data.Map.Base.fromList GHC.Types.[]) (Data.Map.Base.fromList GHC.Types.[])" (shortened)
-- Is something wrong with the Data instance for Map?
-- TODO: FIX this
saltModule :: QuasiQuoter
saltModule = makeQQ $ \str ->
  (buildSaltModuleFromDecls <$> runParserQ program "<quasi-quoted module>" str)
  >>= check
  where
    check (Left msg) = fail $ "Error when building module from quasi quote:\n`" ++ show msg ++"`\n"
    check (Right m) = return m
