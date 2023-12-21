{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module STT.Pretty
  ( Encoding(..)
  , Ann(..)
  , SepByStyle(..)
  , prettyExpr
  , prettyTy
  , prettyDecl
  ) where

import Prettyprinter (Doc, Pretty, pretty, (<+>))
import qualified Prettyprinter as P
import Data.Text (Text)
import Data.Bifunctor (first)
import STT.Syntax (Decl(..), Expr(..), Ty(..), ArithOp(..), CmpOp(..))
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
  pretty = P.unAnnotate . prettyExpr Unicode

prettyExpr :: Encoding -> Expr -> Doc Ann
prettyExpr enc = \case
    ELet x e1 e2  -> prettyELet enc x e1 e2
    EFn x e       -> prettyEFn enc x e
    EFix e        -> prettyEFix enc e
    ECase e cases -> prettyECase enc e cases
    e             -> prettyEAnn enc e

prettyECase :: Encoding -> Expr -> [(Ty, Expr)] -> Doc Ann
prettyECase enc e cases =
  P.hang 0 $ P.vsep $
    _case <+> prettyExpr enc e <+> _of : map each cases
  where
    _case = P.annotate AnnKeyword "case"
    _of   = P.annotate AnnKeyword "of"

    each :: (Ty, Expr) -> Doc Ann
    each (t, e') =
      P.group $ P.nest 4 $ P.vsep
        [ "|" <+> prettyTy enc Naked t <+> "=>"
        , prettyExpr enc e'
        ]

prettyELet :: Encoding -> Text -> Expr -> Expr -> Doc Ann
prettyELet enc x e1 e2 =
  P.vsep
    [ _let <+> pretty x <+> "=" <+> prettyExpr enc e1 <+> _in
    , prettyExpr enc e2
    ]
  where
    _let = P.annotate AnnKeyword "let"
    _in  = P.annotate AnnKeyword "in"

prettyEFix :: Encoding -> Expr -> Doc Ann
prettyEFix enc = \case
    EFn x e -> _fix <+> prettyEFn enc x e
    e       -> _fix <+> prettyTerm enc e
  where
    _fix = P.annotate AnnKeyword "fix"

prettyEFn :: Encoding -> Text -> Expr -> Doc Ann
prettyEFn enc = \x e -> uncurry go $ collect (EFn x e)
  where
    go :: [Text] -> Expr -> Doc Ann
    go xs e = P.nest 2 $ P.sep
      [ lambda <> P.hsep (map pretty xs) <> ","
      , prettyExpr enc e
      ]

    lambda :: Doc Ann
    lambda = case enc of
      Ascii   -> P.annotate AnnKeyword "\\"
      Unicode -> P.annotate AnnKeyword "λ"

    collect :: Expr -> ([Text], Expr)
    collect (EFn x e) = first (x :) (collect e)
    collect e = ([], e)

prettyEAnn :: Encoding -> Expr -> Doc Ann
prettyEAnn enc = \case
  EAnn e t -> prettyECmp enc e <+> ":" <+> prettyTy enc Hang t
  e        -> prettyECmp enc e

prettyECmp :: Encoding -> Expr -> Doc Ann
prettyECmp enc = \case
  ECmp (OpLT e1 e2) -> prettyECmp enc e1 <+> "<"  <+> prettyECmp enc e2 
  ECmp (OpLE e1 e2) -> prettyECmp enc e1 <+> "<=" <+> prettyECmp enc e2 
  ECmp (OpEQ e1 e2) -> prettyECmp enc e1 <+> "==" <+> prettyECmp enc e2 
  ECmp (OpGE e1 e2) -> prettyECmp enc e1 <+> ">=" <+> prettyECmp enc e2 
  ECmp (OpGT e1 e2) -> prettyECmp enc e1 <+> ">"  <+> prettyECmp enc e2
  e                 -> prettyEArith enc e

prettyEArith :: Encoding -> Expr -> Doc Ann
prettyEArith enc = level1
  where
    level1 = \case
      EArith (OpAdd e1 e2) -> level1 e1 <+> "+" <+> level1 e2
      EArith (OpSub e1 e2) -> level1 e1 <+> "-" <+> level1 e2
      e -> level2 e
    level2 = \case
      EArith (OpMul e1 e2) -> level2 e1 <+> "*" <+> level2 e2
      EArith (OpDiv e1 e2) -> level2 e1 <+> "/" <+> level2 e2
      e -> prettyEApp enc e

prettyEApp :: Encoding -> Expr -> Doc Ann
prettyEApp enc = \case
  EApp e1 e2 -> prettyEApp enc e1 <+> prettyTerm enc e2
  EFst e     -> "fst" <+> prettyTerm enc e
  ESnd e     -> "snd" <+> prettyTerm enc e
  e          -> prettyTerm enc e

prettyTerm :: Encoding -> Expr -> Doc Ann
prettyTerm enc e = case e of
  EUnit             -> P.annotate AnnLiteral "()"
  EBool True        -> P.annotate AnnLiteral "true"
  EBool False       -> P.annotate AnnLiteral "false"
  EInt x            -> P.annotate AnnLiteral (pretty x)
  EVar x n | n == 0 -> pretty x
  EVar x n          -> pretty x <> "@" <> pretty n
  EPair e1 e2       -> P.align $ P.tupled [prettyExpr enc e1, prettyExpr enc e2]
  ELet{}            -> P.parens $ prettyExpr enc e
  EFn{}             -> P.parens $ prettyExpr enc e
  EFix{}            -> P.parens $ prettyExpr enc e
  ECase{}           -> P.parens $ prettyExpr enc e
  EAnn{}            -> P.parens $ prettyExpr enc e
  EApp{}            -> P.parens $ prettyExpr enc e
  EFst{}            -> P.parens $ prettyExpr enc e
  ESnd{}            -> P.parens $ prettyExpr enc e
  EArith{}          -> P.parens $ prettyExpr enc e
  ECmp{}            -> P.parens $ prettyExpr enc e

-- ----------------------------------------------------------------------------
-- Pretty Declarations

instance Pretty Decl where
  pretty = P.unAnnotate . prettyDecl Unicode

prettyDecl :: Encoding -> Decl -> Doc Ann
prettyDecl enc = \case
  DDef x e ->
    let def = P.annotate AnnKeyword "def" in
    def <+> pretty x <+> "=" <+> prettyExpr enc e
  DSig x t ->
    let sig = P.annotate AnnKeyword "sig" in
    sig <+> pretty x <+> ":" <+> prettyTy enc Hang t
