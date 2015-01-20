{-# LANGUAGE LambdaCase #-}
module Language.SaLT.Pretty
  ( prettyModule
  , prettyBinding
  , prettyExp
  , prettyTypeInstantiations
  , prettyAlt
  , module FunLogic.Core.Pretty
  ) where

import           Control.Lens
import qualified Data.Map                     as M
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           FunLogic.Core.AST
import           FunLogic.Core.Pretty
import           Language.SaLT.AST

prettyModule :: Module -> Doc
prettyModule m =
  foldr (\a b -> prettyADT a <> line <> line <> b) empty (m^.modADTs.to M.elems)
  <> foldr (\a b -> prettyBinding a <> line <> line <> b) empty (m^.modBinds.to M.elems)

prettyBinding :: Binding -> Doc
prettyBinding b =
  nest defaultIndent (
    text (b^.bindingName) </> text ":: " <> prettyTyDecl (b^.bindingType)
  ) <> line <>
  nest defaultIndent (
    text (b^.bindingName) </> text "= " <> prettyExp (b^.bindingExpr)
  )

prettyExp :: Exp -> Doc
prettyExp ex = case ex of
  EVar v -> text v
  EFun f tys -> text f <> prettyTypeInstantiations tys
  ELam v ty e -> char '\\'
    -- Type annotation with required precedence tyConPrec because we don't want
    -- something like "\f :: a -> b -> f x"
    <> hang defaultIndent (text v <> text " :: " <> withPrec tyConPrec prettyType ty)
    <+> text "->" </> prettyExp e
  -- Nested application needs parentheses on the right hand side. Thus the
  -- higher precedence requirement.
  EApp e1 e2 -> withPrec prc prettyExp e1 </> withPrec (prc + 1) prettyExp e2
  ELit l -> case l of
    LNat i -> text $ show i
  EPrim p exps -> case (p, exps) of
    -- Here it is assumed that addition is associative although this doesn't hold for
    -- e.g. floating point numbers. Otherwise we'd need more parentheses.
    -- TODO: Discuss whether this is okay.
    (PrimAdd, [e1, e2]) -> withPrec prc prettyExp e1 </> char '+' </> withPrec prc prettyExp e2
    -- Nested "==" always require parentheses for clarity.
    (PrimEq, [e1, e2]) -> withPrec (prc + 1) prettyExp e1 </> text "==" </> withPrec (prc + 1) prettyExp e2
    -- Bind requires parentheses only on the left hand side.
    (PrimBind, [e, f]) -> withPrec prc prettyExp e </> text ">>=" </> withPrec 0 prettyExp f
    _ -> error $ "Invalid primitive operation `" ++ show p ++ "` with arguments " ++ show exps ++ "."
  ECon c tys -> hang defaultIndent $ text c <> prettyTypeInstantiations tys
  ESet e -> char '{' <//> prettyExp e <//> char '}'
  ECase e alts -> keyword "case" </> withPrec prc prettyExp e <+> keyword "of"
     <> prettyAlts alts
  EFailed ty -> keyword "failed" <> prettyTypeInstantiations [ty]
  EUnknown ty -> keyword "unknown" <> prettyTypeInstantiations [ty]
  where
    prc = prec ex -- precedence of the current expression
    prettyAlts [] = text " {}"
    prettyAlts alts = nest defaultIndent (line <> align (foldr1 (\a b -> a <> line <> b) $ map prettyAlt alts))

-- | This pretty prints the type instantiations for a polymorphic function.
-- It prints "<::>" for a monomorphic function (although that is not necessary)
-- to make it clear that it is a function and not a local variable.
prettyTypeInstantiations :: [Type] -> Doc
prettyTypeInstantiations = encloseSep (text "<:") (text ":>") (char ',') . map prettyType

prettyAlt :: Alt -> Doc
prettyAlt (Alt pat e) = hang defaultIndent $
  prettyPat pat <> text " ->" </> prettyExp e

prettyPat :: Pat -> Doc
prettyPat = \case
  PVar v -> text v
  PCon c vs -> case vs of
    [] -> text c
    _  -> hang defaultIndent $ text c </> fillSep (map text vs)
