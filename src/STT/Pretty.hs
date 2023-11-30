{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module STT.Pretty
  ( Encoding(..)
  , Ann(..)
  , SepByStyle(..)
  , prettyExpr
  , prettyEAnn
  , prettyTy
  , prettyDecl
  ) where

import Prettyprinter (Doc, Pretty, pretty, (<+>))
import qualified Prettyprinter as P
import Data.Text (Text)
import Data.Bifunctor (first)
import STT.Syntax (Decl(..), Expr(..), Ty(..))
import STT.Pretty.SepBy (SepByStyle(..), sepBy)

-- | todo: ...
data Encoding
  = Ascii
  | Unicode
  deriving (Eq, Show)

-- | Pretty printer document annotations.
data Ann
  = AnnKeyword
  | AnnLiteral
  | AnnTypeName

-- ----------------------------------------------------------------------------
-- Pretty Types

instance Pretty Ty where
  pretty = P.unAnnotate . prettyTy Unicode Naked

prettyTy :: Encoding -> SepByStyle -> Ty -> Doc Ann
prettyTy enc style t = case t of
  TAnd{}  -> prettyTAnd enc style t
  TOr{}   -> prettyTOr enc style t
  TDiff{} -> prettyTDiff enc style t
  TFn{}   -> prettyTFn enc style t
  _       -> prettyTNot enc t

prettyTDiff :: Encoding -> SepByStyle -> Ty -> Doc Ann
prettyTDiff enc style a =
    sepBy style "\\" (prettyTOr enc Naked <$> reverse (collect a))
  where
    collect :: Ty -> [Ty]
    collect (TDiff t u) = u : collect t
    collect t = [t]

prettyTOr :: Encoding -> SepByStyle -> Ty -> Doc Ann
prettyTOr enc style a =
    sepBy style delim (prettyTAnd enc Naked <$> collect a)
  where
    delim :: Text
    delim = case enc of
      Ascii   -> "|"
      Unicode -> "∨"

    collect :: Ty -> [Ty]
    collect (TOr t u) = collect t ++ collect u
    collect t = [t]

prettyTAnd :: Encoding -> SepByStyle -> Ty -> Doc Ann
prettyTAnd enc style a =
    sepBy style delim (prettyTNot enc <$> collect a)
  where
    delim :: Text
    delim = case enc of
      Ascii   -> "&"
      Unicode -> "∧"

    collect :: Ty -> [Ty]
    collect (TAnd t u) = collect t ++ collect u
    collect t = [t]

prettyTNot :: Encoding -> Ty -> Doc Ann
prettyTNot enc = \case
    TNot t -> pretty prefix <> prettyTNot enc t
    t      -> prettyTBase enc t
  where
    prefix :: Text
    prefix = case enc of
      Ascii   -> "~"
      Unicode -> "¬"

prettyTFn :: Encoding -> SepByStyle -> Ty -> Doc Ann
prettyTFn enc style a =
    sepBy style delim (prettyTNot enc <$> collect a)
  where
    delim :: Text
    delim = case enc of
      Ascii   -> "->"
      Unicode -> "→"

    collect :: Ty -> [Ty]
    collect (TFn t u) = t : collect u
    collect t = [t]

prettyTEmpty :: Encoding -> Doc Ann
prettyTEmpty = \case
  Ascii   -> "Empty"
  Unicode -> "⊥"

prettyTAny :: Encoding -> Doc Ann
prettyTAny = \case
  Ascii   -> "Any"
  Unicode -> "⊤"

prettyTBase :: Encoding -> Ty -> Doc Ann
prettyTBase enc t = case t of
  TEmpty             -> P.annotate AnnTypeName (prettyTEmpty enc)
  TAny               -> P.annotate AnnTypeName (prettyTAny enc)
  TUnit              -> P.annotate AnnTypeName "()"
  TBool (Just True)  -> P.annotate AnnTypeName "true"
  TBool (Just False) -> P.annotate AnnTypeName "false"
  TBool Nothing      -> P.annotate AnnTypeName "Bool"
  TInt (Just x)      -> P.annotate AnnTypeName (pretty x)
  TInt Nothing       -> P.annotate AnnTypeName "Int"
  TPair t1 t2        -> P.align $ P.tupled [pretty t1, pretty t2]
  TFn{}              -> prettyTFn enc Paren t
  TAnd{}             -> prettyTAnd enc Paren t
  TOr{}              -> prettyTOr enc Paren t
  TDiff{}            -> prettyTDiff enc Paren t
  TNot{}             -> P.parens (prettyTNot enc t)

-- ----------------------------------------------------------------------------
-- Pretty Expressions

instance Pretty Expr where
  pretty = P.unAnnotate . prettyEAnn Unicode

prettyEAnn :: Encoding -> Expr -> Doc Ann
prettyEAnn enc = \case
  EAnn e t -> prettyExpr enc e <+> ":" <+> prettyTy enc Hang t
  e        -> prettyExpr enc e

prettyExpr :: Encoding -> Expr -> Doc Ann
prettyExpr enc = \case
    EIf e1 ty e2 e3 -> prettyEIf enc e1 ty e2 e3
    ELet x e1 e2    -> prettyELet enc x e1 e2
    EFn x e         -> uncurry (prettyEFn enc) $ collectArgs (EFn x e)
    e               -> prettyEApp enc e
  where
    collectArgs :: Expr -> ([Text], Expr)
    collectArgs (EFn x e) = first (x :) (collectArgs e)
    collectArgs e = ([], e)

prettyEIf :: Encoding -> Expr -> Ty -> Expr -> Expr -> Doc Ann
prettyEIf enc e1 ty e2 e3 =
  P.nest 2 $ P.vsep
    [ _if <+> prettyExpr enc e1 <+> _is <+> prettyTy enc Hang ty
    , _then <+> prettyExpr enc e2
    , _else <+> prettyExpr enc e3
    ]
  where
    _if   = P.annotate AnnKeyword "if"
    _is   = P.annotate AnnKeyword "is"
    _then = P.annotate AnnKeyword "then"
    _else = P.annotate AnnKeyword "else"

prettyELet :: Encoding -> Text -> Expr -> Expr -> Doc Ann
prettyELet enc x e1 e2 =
  P.vsep
    [ _let <+> pretty x <+> "=" <+> prettyExpr enc e1 <+> _in
    , prettyExpr enc e2
    ]
  where
    _let = P.annotate AnnKeyword "let"
    _in  = P.annotate AnnKeyword "in"

prettyEFn :: Encoding -> [Text] -> Expr -> Doc Ann
prettyEFn enc xs e =
  P.nest 2 $ P.sep
    [ lambda <> P.hsep (map pretty xs) <> ","
    , prettyExpr enc e
    ]
  where
    lambda :: Doc Ann
    lambda = case enc of
      Ascii   -> P.annotate AnnKeyword "\\"
      Unicode -> P.annotate AnnKeyword "λ"

prettyEApp :: Encoding -> Expr -> Doc Ann
prettyEApp enc = \case
  EApp e1 e2 -> prettyEApp enc e1 <+> prettyTerm enc e2
  e          -> prettyTerm enc e

prettyTerm :: Encoding -> Expr -> Doc Ann
prettyTerm enc = \case
  EUnit       -> P.annotate AnnLiteral "()"
  EBool True  -> P.annotate AnnLiteral "true"
  EBool False -> P.annotate AnnLiteral "false"
  EInt x      -> P.annotate AnnLiteral (pretty x)
  EVar x      -> pretty x
  EPair e1 e2 -> P.align $ P.tupled [prettyEAnn enc e1, prettyEAnn enc e2]
  e           -> P.parens $ prettyEAnn enc e

-- ----------------------------------------------------------------------------
-- Pretty Declarations

instance Pretty Decl where
  pretty = P.unAnnotate . prettyDecl Unicode

prettyDecl :: Encoding -> Decl -> Doc Ann
prettyDecl enc (DDef x e) =
  let def = P.annotate AnnKeyword "def" in
  def <+> pretty x <+> "=" <+> prettyExpr enc e
