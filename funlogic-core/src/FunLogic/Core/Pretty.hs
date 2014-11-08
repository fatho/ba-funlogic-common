{-# LANGUAGE PatternSynonyms #-}
module FunLogic.Core.Pretty where

import           Control.Lens
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           FunLogic.Core.AST

defaultIndent :: Int
defaultIndent = 2

keyword :: String -> Doc
keyword = bold . blue . text

prettyADT :: ADT -> Doc
prettyADT adt = hang defaultIndent $
  keyword "data" <+> text (adt^.adtName) <+> tyArgs <//> encloseSep (text "= ") empty (text "| ") constrs
    where
    tyArgs = foldr (<+>) empty . map text $ adt^.adtTyArgs
    constrs = map prettyConDecl $ adt^.adtConstr

prettyConDecl :: ConDecl -> Doc
prettyConDecl (ConDecl con tys) = hang defaultIndent $ text con </> foldr (</>) empty (map prettyType tys)

prettyType :: Type -> Doc
prettyType ty = case ty of
  TVar a     -> text a
  TFun x y   -> parens $ prettyType x </> text "->" <+> prettyType y
  TTup x y   -> tupled [prettyType x, prettyType y]
  TCon c []  -> text c
  TCon c tys -> parens $ text c <+> fillSep (map prettyType tys)

prettyTyDecl :: TyDecl -> Doc
prettyTyDecl (TyDecl vs cs ty) =
  keyword "forall" <+> fillSep (map text vs) <> char '.'
  <> constraints
  </> prettyType ty
  where
    constraints | null cs = empty
                | otherwise = empty </> encloseSep (char '(') (char ')') (text ", ") (map prettyTyConstraint cs) <+> text "=>"

prettyTyConstraint :: TyConstraint -> Doc
prettyTyConstraint (TyConstraint n v) = text n <+> text v

-- | Renders the Doc with the given width and displays it.
displayPretty :: Int -> Doc -> IO ()
displayPretty wd = displayIO stdout . renderPretty 1.0 wd
