{-# LANGUAGE PatternSynonyms #-}
module FunLogic.Core.Pretty where

import           Control.Lens
import           Text.PrettyPrint.Leijen hiding ((<$>))

import           FunLogic.Core.AST

defaultIndent :: Int
defaultIndent = 2

prettyADT :: ADT -> Doc
prettyADT adt = hang defaultIndent $
  text "data" <+> text (adt^.adtName) <+> tyArgs <//> encloseSep (text "= ") empty (text "| ") constrs
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
  text "forall" <+> fillSep (map text vs) <> char '.'
  <> constraints
  </> prettyType ty
  where
    constraints | null cs = empty
                | otherwise = empty </> encloseSep (char '(') (char ')') (text ", ") (map prettyTyConstraint cs) <+> text "=>"

prettyTyConstraint :: TyConstraint -> Doc
prettyTyConstraint (TyConstraint n v) = text n <+> text v

testPretty :: Int -> Doc -> IO ()
testPretty wid = putStr . flip displayS "" . renderPretty 1.0 wid
