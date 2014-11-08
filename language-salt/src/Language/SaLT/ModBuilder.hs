{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.SaLT.ModBuilder where

import           Control.Applicative
import           Control.Lens        hiding (noneOf)
import           Control.Monad.State
import qualified Data.Map            as M

import           Language.SaLT.AST

-- TODO: Nicer looking error messages by using `Doc` from `ansi-wl-pprint`
-- | Builds a module from a list of declarations.
buildModule :: [Decl] -> Either String Module
buildModule decls = fst <$> execStateT (mapM_ collectDecl decls) (emptyModule "Main", M.empty) where
  emptyModule name = Module
    { _modName = name
    , _modBinds = M.empty
    , _modADTs = M.empty
    }
  collectDecl d = case d of
    DTop binding -> let topName = binding^.bindingName
      in use (_1.modBinds.at topName) >>= \case
        Just other -> lift $ Left $ "top-level `" ++ topName ++ "` is redefined at " ++ binding^.bindingSrc.to show
                            ++ ", previously defined at " ++ other^.bindingSrc.to show
        Nothing -> _1.modBinds.at topName .= Just binding

    DData adt@ADT {..} -> use (_1.modADTs.at _adtName) >>= \case
      Just other -> lift $ Left $ "ADT `" ++ _adtName ++ "` is redefined at " ++ show _adtSrcRef
                         ++ ", previously defined at " ++ other^.adtSrcRef.to show
      Nothing -> do
        _1.modADTs.at _adtName .= Just adt
        forM_ _adtConstr $ \(ConDecl n _) -> use (_2.at n) >>= \case
          Just other -> lift $ Left $ concat
            [ "data constructor `", n, "` redefined in `", _adtName, "` at ", show _adtSrcRef
            , ", previously defined in `", other^.adtName, "` at ", other^.adtSrcRef.to show]
          Nothing -> _2.at n .= Just adt
