{-# LANGUAGE LambdaCase #-}

module Main.Repl (repl) where

import Control.Monad.Trans
import Control.Monad.State
import qualified System.Console.Repline as R
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Terminal as Terminal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import STT.Eval
import STT.Parser
import STT.Syntax
import STT.Pretty

type Repl a = R.HaskelineT (StateT ReplState IO) a

-- | The REPL state.
data ReplState = ReplState
  { replEncoding :: Encoding  -- ^ Are we printing ASCII symbols, or unicode?
  , replEnv :: Env            -- ^ Terms bound in the environment.
  }

initState :: ReplState
initState = ReplState
  { replEncoding = Unicode
  , replEnv = Map.empty
  }

-- | The Repl prompt.
banner :: R.MultiLine -> Repl String
banner R.SingleLine = return "stt> "
banner R.MultiLine  = return "...> "

-- | Print the welcome message.
hello :: Repl ()
hello = liftIO $ putStr $ unlines
  [ "Welcome to stt!"
  , "Type :help for more information."
  ]

-- | Repl meta commands.
options :: [(String, String -> Repl ())]
options =
  [ ("ast",     cmdAst)
  , ("env",     cmdEnv)
  , ("h",       cmdHelp)
  , ("help",    cmdHelp)
  , ("l",       cmdLoad)
  , ("load",    cmdLoad)
  , ("pp",      cmdPretty)
  , ("pretty",  cmdPretty)
  , ("reset",   cmdReset)
  , ("debug",   cmdDebug)
  , ("q",       const R.abort)
  , ("quit",    const R.abort)
  , ("unicode", cmdUnicode)
  ]

-- | Print the list of options.
cmdHelp :: String -> Repl ()
cmdHelp _ = mapM_ (\(x, y) -> liftIO $ printf "%-16s %s\n" x y) table
  where
    table :: [(String, String)]
    table =
      [ (":ast",           "Parse an expression and print its AST")
      , (":env",           "Print the current environment")
      , (":h :help",       "Print this message")
      , (":l :load",       "Load definitions from a file")
      , (":paste",         "Read multiple lines of input")
      , (":pp :pretty",    "Pretty print an expression without evaluating it")
      , (":reset",         "Reset the REPL state to an empty environment")
      , (":debug",         "Print all steps of an evaulation")
      , (":q :quit",       "Exit the REPL")
      , (":unicode [y|n]", "Enable/disable unicode output")
      ]

-- | Parse an expression and print its AST.
cmdAst :: String -> Repl ()
cmdAst line =
  R.dontCrash $ do
    e <- unwrap $ parseExpr "repl" line
    liftIO $ print e

-- | Print the current environment.
cmdEnv :: String -> Repl ()
cmdEnv _ = do
    env <- gets replEnv
    mapM_ each (Map.keys env)
  where
    each :: Text -> Repl ()
    each x = liftIO $ putStrLn (T.unpack x)

-- | Pretty print an expression without evaluating it
cmdPretty :: String -> Repl ()
cmdPretty line =
  R.dontCrash $ do
    e <- unwrap $ parseExpr "repl" line
    ppExpr e

-- | Reset the REPL state to an empty environment.
cmdReset :: String -> Repl ()
cmdReset _ = modifyEnv (const Map.empty)

-- | Load definitions from a file.
cmdLoad :: String -> Repl ()
cmdLoad line = mapM_ load (words line)
  where
    load :: String -> Repl ()
    load path = do
      -- TODO: catch execptions when reading.
      liftIO $ putStrLn ("Loading " ++ path ++ "...")
      text <- liftIO (readFile path)
      decls <- unwrap $ parseFile path text
      mapM_ each decls

    each :: Decl -> Repl ()
    each = \case
      DDef x e -> do
        env <- gets replEnv
        let e' = eval env e
        modifyEnv (Map.insert x [e'])
      DSig _ _ ->
        return ()


-- | Enable/disable unicode output.
cmdUnicode :: String -> Repl ()
cmdUnicode line =
  case trim line of
    "y"   -> setEncoding Unicode
    "yes" -> setEncoding Unicode
    "n"   -> setEncoding Ascii
    "no"  -> setEncoding Ascii
    ""    -> showCurrent
    _     -> liftIO $ putStrLn "'yes' or 'no', please."
  where
    setEncoding :: Encoding -> Repl ()
    setEncoding enc = modify $ \s -> s { replEncoding = enc }

    showCurrent :: Repl ()
    showCurrent = do
      enc <- gets replEncoding
      case enc of
        Unicode -> liftIO $ putStrLn "yes"
        Ascii   -> liftIO $ putStrLn "no"

-- | Handle a line of input from the repl.
cmdDebug :: String -> Repl ()
cmdDebug line =
  R.dontCrash $ do
    expr <- unwrap $ parseExpr "repl" line
    env  <- gets replEnv
    mapM_ ppExpr (expr : steps env expr)

-- | Handle a line of input from the repl.
cmd :: String -> Repl ()
cmd line =
  R.dontCrash $ do
    s <- unwrap $ parseToplevel "repl" line
    case s of
      Right expr -> do
        env <- gets replEnv
        ppExpr (eval env expr)
      Left (DDef x e) -> do
        env <- gets replEnv
        let e' = eval env e
        modifyEnv (Map.insert x [e'])
      Left (DSig _ _) ->
        return ()

-- | Return auto-complete suggestions for a given identifier.
complete :: String -> StateT ReplState IO [String]
complete s = do
  env <- gets replEnv
  let names = filter (T.isPrefixOf $ T.pack s) (Map.keys env)
  return $ map T.unpack names

repl :: IO ()
repl = flip evalStateT initState $ R.evalReplOpts $ R.ReplOpts
  { R.banner           = banner
  , R.command          = cmd
  , R.options          = options
  , R.prefix           = Just ':'
  , R.multilineCommand = Just "paste"
  , R.tabComplete      = R.Word0 complete
  , R.initialiser      = hello
  , R.finaliser        = return R.Exit
  }

-- ---------------------------------------------------------------------------
-- Pretty Printing

-- | Convert pretty printer annotations into terminal colors.
ansify :: Ann -> Terminal.AnsiStyle
ansify AnnKeyword  = Terminal.color Terminal.Blue
ansify AnnLiteral  = Terminal.color Terminal.Red
ansify AnnTypeName = Terminal.color Terminal.Cyan

-- | Pretty print an expression.
ppExpr :: Expr -> Repl ()
ppExpr x = do
  enc <- gets replEncoding
  liftIO $ do
    Terminal.putDoc (P.reAnnotate ansify $ prettyExpr enc x)
    putStrLn ""
    hFlush stdout

-- ---------------------------------------------------------------------------
-- Miscellaneous

trim :: String -> String
trim = unwords . words

-- | Unwrap an 'Either'. Abort if 'Left'.
unwrap :: Show a => Either a b -> Repl b
unwrap (Right x) = return x
unwrap (Left err) = liftIO (print err) >> R.abort

-- | Update the environment.
modifyEnv :: (Env -> Env) -> Repl ()
modifyEnv f = modify $ \s -> s { replEnv = f (replEnv s) }
