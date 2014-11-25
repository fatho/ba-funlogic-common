{-# LANGUAGE LambdaCase #-}
module Language.CuMin.ModBuilder where

import           Control.Monad.Writer
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Result

import           FunLogic.Core.ModBuilder
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