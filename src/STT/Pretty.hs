{-# LANGUAGE OverloadedStrings #-}

module STT.Pretty where

import Prettyprinter (Doc, Pretty, pretty, (<+>))
import qualified Prettyprinter as P
import STT.Syntax

prettyFn :: Expr -> Doc ann
prettyFn (EFn x e) =
  "\\" <> pretty x <+> "->" <+> prettyFn e
prettyFn e = prettyApp e

prettyApp :: Expr -> Doc ann
prettyApp (EApp e1 e2) =
  prettyApp e1 <+> prettyTerm e2
prettyApp e = prettyTerm e

prettyTerm :: Expr -> Doc ann
prettyTerm (EBool x) = prettyBool x
prettyTerm (EInt x) = pretty x
prettyTerm (EVar x) = pretty x
prettyTerm (EPair x y) = prettyPair x y
prettyTerm e = P.parens $ pretty e

prettyPair :: Expr -> Expr -> Doc ann
prettyPair x y = P.group $ P.encloseSep l r s [pretty x, pretty y]
  where
    l = P.flatAlt "( " "("
    r = P.flatAlt " )" ")"
    s = ", "

prettyBool :: Bool -> Doc ann
prettyBool True = "true"
prettyBool False = "false"

instance Pretty Expr where
  pretty = prettyFn

prettyDef :: Decl -> Doc ann
prettyDef (DDef x e) =
  "def" <+> pretty x <+> "=" <+> pretty e

instance Pretty Decl where
  pretty = prettyDef
