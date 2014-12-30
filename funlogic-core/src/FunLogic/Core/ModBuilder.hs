{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}
module FunLogic.Core.ModBuilder where

import           Control.Applicative
import           Control.Lens                 hiding (noneOf)
import           Control.Monad.State
import           Data.Function
import qualified Data.Map                     as M
import           Data.Monoid
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

import           FunLogic.Core.AST

-- | Builds a module from a list of declarations.
buildModule :: IsBinding b => String -> [ADT] -> [b] -> Either Doc (CoreModule b)
buildModule name adts bindings = fst <$> execStateT (mapM_ collectADT adts >> mapM_ collectBinding bindings) (emptyModule name, M.empty) where
  collectBinding binding = let topName = binding^.bindingName
      in use (_1.modBinds.at topName) >>= \case
        Just other -> lift $ Left $ text "top-level `" <> dullyellow (text topName)
          <> text "` is redefined at " <> dullyellow (text $ binding^.bindingSrc.to show)
          <> text ", previously defined at " <> dullyellow (text $ other^.bindingSrc.to show)
        Nothing -> _1.modBinds.at topName .= Just binding

  collectADT adt@ADT {..} = use (_1.modADTs.at _adtName) >>= \case
      Just other -> lift $ Left $ text "ADT `" <> dullyellow (text _adtName)
        <> text "` is redefined at " <> dullyellow (text $ show _adtSrcRef)
        <> text ", previously defined at " <> dullyellow (text $ other^.adtSrcRef.to show)
      Nothing -> do
        _1.modADTs.at _adtName .= Just adt
        forM_ _adtConstr $ \(ConDecl n _) -> use (_2.at n) >>= \case
          Just other -> lift $ Left $ mconcat
            [text "data constructor `", dullyellow $ text n, text "` redefined in `"
            , dullyellow $ text _adtName, text "` at ", dullyellow $ text  $ show _adtSrcRef
            , text ", previously defined in `", dullyellow $ text $ other^.adtName
            , text "` at ", dullyellow $ text $ other^.adtSrcRef.to show]
          Nothing -> _2.at n .= Just adt

-- | Returns an empty module with the given name
emptyModule :: String -> CoreModule b
emptyModule name = CoreModule
  { _modName = name
  , _modBinds = M.empty
  , _modADTs = M.empty
  }

-- | Merges the second argument into the first one, if the names of bindings and data types are disjoint.
-- Returns either the ambigous data types and bindings or a new merged module.
importUnqualified :: CoreModule b -> CoreModule b -> Either (M.Map TyConName ADT, M.Map BindingName b) (CoreModule b)
importUnqualified modul importMod
    | M.null commonBindings && M.null commonADTs = Right $
      modul & modBinds %~ M.union (view modBinds importMod)
          & modADTs  %~ M.union (view modADTs importMod)
    | otherwise = Left (commonADTs, commonBindings)
  where
    commonBindings = (M.intersection `on` view modBinds) modul importMod
    commonADTs     = (M.intersection `on` view modADTs)  modul importMod
