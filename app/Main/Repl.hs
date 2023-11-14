module Main.Repl (repl) where

import Data.Char (isSpace)
import STT.Parser (parseExpr)
import System.Console.Haskeline

-- ----------------------------------------------------------------------------
-- Utilities

type Repl a = InputT IO a

-- | The default REPL prompt.
prompt :: String
prompt = "stti> "

-- | Run the REPL.
repl :: IO ()
repl = runInputT defaultSettings (banner >> loop)
  where
    banner :: Repl ()
    banner = outputStrLn "Type :help for more information"

-- | The main loop.
loop :: Repl ()
loop = do
  mline <- getInputLine prompt
  case mline of
    Nothing   -> return ()
    Just line -> do
      case splitOnMeta line of
        Just (":h", _)    -> cmdHelp
        Just (":help", _) -> cmdHelp
        Just (":q", _)    -> return ()
        Just (":quit", _) -> return ()
        Just (meta, _)    -> outputStrLn $ "Unknown meta command " ++ meta
        Nothing           -> cmdDefault line
      loop

-- ----------------------------------------------------------------------------
-- Commands

-- | Print the REPL help message.
cmdHelp :: Repl ()
cmdHelp = do
  outputStrLn ":h[elp]         Print this message"
  outputStrLn ":q[uit]         Exit the REPL"

cmdDefault :: String -> Repl ()
cmdDefault line = do
  case parseExpr "repl" line of
    Left e -> outputStrLn $ show e
    Right x -> outputStrLn $ show x

-- ----------------------------------------------------------------------------
-- Utilities

-- | Given a line of input from the REPL, parse the first word and see if it
-- starts with a colon. If so, return a pair containing the first word (i.e.,
-- the meta command) and the remainder of the input.
splitOnMeta :: String -> Maybe (String, String)
splitOnMeta line =
  case break isSpace (dropWhile isSpace line) of
    p@(':':_, _) -> Just p
    _            -> Nothing
