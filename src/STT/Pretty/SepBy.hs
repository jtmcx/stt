{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module STT.Pretty.SepBy
  ( SepByStyle(..)
  , sepBy
  , nakedSepBy
  , hangSepBy
  , parenSepBy
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter (Doc, pretty, (<+>))
import qualified Prettyprinter as P

data SepByStyle
  = Naked
  | Paren
  | Hang


-- | Invoke 'nakedSepBy', 'parenSepBy', or 'hangSepBy', depending on the
-- value of 'SepByStyle'.
sepBy :: SepByStyle -> Text -> [Doc ann] -> Doc ann
sepBy = \case
  Naked -> nakedSepBy
  Paren -> parenSepBy
  Hang  -> hangSepBy


-- | Join a sequence of documents together with a delimiter. The documents are
-- joined using @group . vep@. If the output is too narrow, the documents are
-- rendered line-by-line, each prefixed with the delimiter (except the first).
--
-- >>> let doc = "prefix :" <+> nakedSepBy "->" ["one", "two", "three"]
-- >>> putDocW 80 doc
-- prefix : one -> two -> three
--
-- >>> putDocW 20 doc
-- prefix : one
-- -> two
-- -> three
nakedSepBy :: Text -> [Doc ann] -> Doc ann
nakedSepBy _ []       = mempty
nakedSepBy p (x : xs) = P.sep $ x : map (pretty p <+>) xs


-- | Similar to 'nakedSepBy', but hang the delimiter underneath the current
-- column. The subdocuments are grouped (i.e., they are automatically inlined if
-- the output is wide enough).
--
-- >>> let doc = "prefix :" <+> hangSepBy "->" ["one", "two", "three"]
-- >>> putDocW 80 doc
-- prefix : one -> two -> three
--
-- >>> putDocW 20 doc
-- prefix : one
--       -> two
--       -> three
hangSepBy :: Text -> [Doc ann] -> Doc ann
hangSepBy p xs = P.hang (-1 - T.length p) $ nakedSepBy p xs


-- | Similar to 'nakedSepBy', but wrap the document in parentheses. The
-- subdocuments are grouped (i.e., they are automatically inlined if the output
-- is wide enough). The output document is aligned with the opening parenthesis.
--
-- >>> let doc = "prefix :" <+> parenSepBy "->" ["one", "two", "three"]
-- >>> putDocW 80 doc
-- prefix : (one -> two -> three)
--
-- >>> putDocW 20 doc
-- prefix : (  one
--          -> two
--          -> three
--          )
parenSepBy :: Text -> [Doc ann] -> Doc ann
parenSepBy prefix xs = P.group $ P.align $ l <> nakedSepBy prefix xs <> r
  where
    l = let spaces = T.replicate (T.length prefix) " "
        in P.flatAlt ("(" <> pretty spaces) "("
    r = P.line' <> ")"
