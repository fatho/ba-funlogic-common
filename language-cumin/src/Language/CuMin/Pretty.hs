{-# LANGUAGE LambdaCase #-}
module Language.CuMin.Pretty where

import           Control.Lens
import qualified Data.Map                     as M
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           FunLogic.Core.AST
import           FunLogic.Core.Pretty
import           Language.CuMin.AST

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
    text (b^.bindingName)
    </> foldr ((</>) . text) empty (b^.bindingArgs)
    <//> text "= "
    <> prettyExp (b^.bindingExpr)
  )

prettyExp :: Exp -> Doc
prettyExp ex = case ex of
  EVar v -> text v
  EFun f tys -> text f <> prettyTypeInstantiations tys
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
    _ -> error $ "Invalid primitive operation `" ++ show p ++ "` with arguments " ++ show exps ++ "."
  ECon c tys -> hang defaultIndent $ text c <> prettyTypeInstantiations tys
  ECase e alts -> keyword "case" </> withPrec prc prettyExp e <+> keyword "of"
     <> nest defaultIndent (line <> align (foldr1 (\a b -> a <> line <> b) $ map prettyAlt alts))
  ELet v e body -> keyword "let" </> hang 2 (text v <+> text "=" <+> prettyExp e)
    </> keyword "in" </> prettyExp body
  ELetFree v ty body -> keyword "let" </> hang 2 (text v <+> text "::" <+> prettyType ty <+> text "free")
    </> keyword "in" </> prettyExp body
  EFailed ty -> keyword "failed" <> prettyTypeInstantiations [ty]
  where
    prc = prec ex -- precedence of the current expression

prettyTypeInstantiations :: [Type] -> Doc
prettyTypeInstantiations = \case
  [] -> empty
  tys -> encloseSep (text "<:") (text ":>") (char ',') $ map prettyType tys

prettyAlt :: Alt -> Doc
prettyAlt (Alt pat e) = hang defaultIndent $
  prettyPat pat <> text " ->" </> prettyExp e

prettyPat :: Pat -> Doc
prettyPat = \case
  PVar v -> text v
  PCon c vs -> case vs of
    [] -> text c
    _  -> hang defaultIndent $ text c </> fillSep (map text vs)
