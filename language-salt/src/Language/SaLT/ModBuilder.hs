{-# LANGUAGE LambdaCase #-}
module Language.SaLT.ModBuilder where

import           Control.Monad.Writer
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Result

import           FunLogic.Core.ModBuilder
import           Language.SaLT.AST
import           Language.SaLT.Parser
import           Language.SaLT.Prelude

buildSaltModuleFromFile :: MonadIO m => String -> m (Either Doc Module)
buildSaltModuleFromFile saltFile = parseSaltFileEx saltFile >>= \case
    Failure msg -> return $ Left msg
    Success decls -> buildSaltModuleFromDecls decls

buildSaltModuleFromDecls :: MonadIO m => [Decl] -> m (Either Doc Module)
buildSaltModuleFromDecls decls =
  let
    adts = preludeADTs ++ [adt | DData adt <- decls]
    bnds = preludeBindings ++ [bnd | DTop bnd <- decls]
  in return $ buildModule "Main" adts bnds
