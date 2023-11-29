{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module STT.Pretty () where

import Prettyprinter (Doc, Pretty, pretty, (<+>))
import qualified Prettyprinter as P
import STT.Syntax (Decl(..), Expr(..))

-- ----------------------------------------------------------------------------
-- Pretty Expressions

instance Pretty Expr where
  pretty = prettyExpr

prettyExpr :: Expr -> Doc ann
prettyExpr = \case
  EFn x e      -> "λ" <> pretty x <> "," <+> pretty e
  ELet x e1 e2 -> "let" <> pretty x <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
  e            -> prettyApp e

prettyApp :: Expr -> Doc ann
prettyApp (EApp e1 e2) = prettyApp e1 <+> prettyTerm e2
prettyApp e = prettyTerm e

prettyTerm :: Expr -> Doc ann
prettyTerm = \case
  EUnit       -> "()"
  EBool True  -> "true"
  EBool False -> "false"
  EInt x      -> pretty x
  EVar x      -> pretty x
  EPair e1 e2 -> P.align $ P.tupled [pretty e1, pretty e2]
  e           -> P.parens $ pretty e

-- ----------------------------------------------------------------------------
-- Pretty Declarations

instance Pretty Decl where
  pretty = prettyDecl

prettyDecl :: Decl -> Doc ann
prettyDecl (DDef x e) =
  "def" <+> pretty x <+> "=" <+> pretty e
