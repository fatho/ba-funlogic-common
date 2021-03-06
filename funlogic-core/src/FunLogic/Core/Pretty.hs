{-# LANGUAGE PatternSynonyms #-}
module FunLogic.Core.Pretty where

import           Control.Lens
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           FunLogic.Core.AST

-- | Adds parentheses if the precedence is lower than the
-- given required precedence.
withPrec :: HasPrecedence a => Int -> (a -> Doc) -> a -> Doc
withPrec requiredPrec prettifier e =
  (if prec e < requiredPrec then parens else id) $ prettifier e

-- | Default indentation of code. Currently 2.
defaultIndent :: Int
defaultIndent = 2

-- | Highlighting for keywords.
keyword :: String -> Doc
keyword = bold . blue . text

-- | Pretty prints and ADT.
prettyADT :: ADT -> Doc
prettyADT adt = hang defaultIndent $
  keyword "data" <+> text (adt^.adtName) <+> tyArgs </> encloseSep (text "= ") empty (text "| ") constrs
    where
    tyArgs = fillSep . map text $ adt^.adtTyArgs
    constrs = map (\c -> prettyConDecl c <+> empty) $ adt^.adtConstr

-- | Pretty prints a constructor declaration.
prettyConDecl :: ConDecl -> Doc
prettyConDecl (ConDecl con tys) = case tys of
  [] -> text con
  _  -> hang defaultIndent $ text con </> fillSep (map (withPrec maxTypePrec prettyType) tys)

-- | Pretty prints a type.
prettyType :: Type -> Doc
prettyType ty = case ty of
  TVar a     -> text a
  -- On the left hand side of "->" qarentheses for function types are required.
  -- On the right hand side they're not. Thus the difference in required precedence:
  TFun x y   -> withPrec (prc + 1) prettyType x </> text "->" <+> withPrec prc prettyType y
  TCon c []  -> text c
  TCon c tys -> text c <+> fillSep (map (withPrec maxTypePrec prettyType) tys)
  where
    prc = prec ty

-- | Pretty prints a type declaration.
prettyTyDecl :: TyDecl -> Doc
prettyTyDecl (TyDecl vs cs ty) =
  quantifications
  <> constraints
  <> prettyType ty
  where
    quantifications | null vs = empty
                    | otherwise = keyword "forall" <+> fillSep (map text vs) <> char '.' </> empty
    constraints | null cs = empty
                | otherwise = encloseSep (char '(') (char ')') (text ", ") (map prettyTyConstraint cs) <+> text "=>" </> empty

-- | Pretty prints a type constraint.
prettyTyConstraint :: TyConstraint -> Doc
prettyTyConstraint (TyConstraint n v) = text n <+> text v

-- | Renders the Doc with the given width and displays it.
displayPretty :: Int -> Doc -> IO ()
displayPretty wd = displayIO stdout . renderPretty 0.7 wd

-- | Removes all color from the Doc
removeFormatting :: Doc -> Doc
removeFormatting = plain
