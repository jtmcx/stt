{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module STT.Pretty where

import Prettyprinter (Doc, Pretty, pretty, (<+>))
import qualified Prettyprinter as P
import STT.Syntax (Decl(..), Expr(..))

-- ----------------------------------------------------------------------------
-- Pretty Expressions

instance Pretty Expr where
  pretty = prettyExpr

prettyExpr :: Expr -> Doc ann
prettyExpr e = case e of
  EFn x e'     -> "λ" <> pretty x <> "," <+> pretty e'
  ELet x e1 e2 -> "let" <> pretty x <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
  _            -> prettyApp e

prettyApp :: Expr -> Doc ann
prettyApp (EApp e1 e2) = prettyApp e1 <+> prettyTerm e2
prettyApp e = prettyTerm e

prettyTerm :: Expr -> Doc ann
prettyTerm e = case e of
  EBool True  -> "true"
  EBool False -> "false"
  EInt x      -> pretty x
  EVar x      -> pretty x
  EPair x y   -> prettyPair x y
  _           -> P.parens $ pretty e

prettyPair :: Expr -> Expr -> Doc ann
prettyPair x y =
    P.group $ P.encloseSep open close sep [pretty x, pretty y]
  where
    open  = P.flatAlt "( " "("
    close = P.flatAlt " )" ")"
    sep   = ", "

-- ----------------------------------------------------------------------------
-- Pretty Declarations

instance Pretty Decl where
  pretty = prettyDecl

prettyDecl :: Decl -> Doc ann
prettyDecl (DDef x e) =
  "def" <+> pretty x <+> "=" <+> pretty e
