module Main.Repl (repl) where

import Control.Monad.Trans
import Control.Monad.State
import qualified System.Console.Repline as R
import Prettyprinter (Pretty)
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Text.Printf (printf)

import STT.Eval
import STT.Parser
import STT.Syntax
import STT.Pretty ()

type Repl a = R.HaskelineT (StateT Env IO) a

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
  [ ("ast",  cmdAst)
  , ("env",  cmdEnv)
  , ("h",    cmdHelp)
  , ("help", cmdHelp)
  , ("l",    cmdLoad)
  , ("load", cmdLoad)
  , ("q",    const R.abort)
  , ("quit", const R.abort)
  ]

-- | Print the list of options.
cmdHelp :: String -> Repl ()
cmdHelp _ = mapM_ (\(x, y) -> liftIO $ printf "%-16s %s\n" x y) table
  where
    table :: [(String, String)]
    table =
      [ (":ast",    "Parse an expression and print the AST")
      , (":env",    "Print the current environment")
      , (":h[elp]", "Print this message")
      , (":paste",  "Read multiple lines of input")
      , (":q[uit]", "Exit the REPL")
      ]

-- | Parse an expression and print the AST.
cmdAst :: String -> Repl ()
cmdAst line =
  R.dontCrash $ do
    e <- unwrap $ parseExpr "repl" line
    liftIO $ print e

-- | Print the current environment.
cmdEnv :: String -> Repl ()
cmdEnv _ = do
    env <- get
    mapM_ (each env) (Map.toList env)
  where
    each :: Env -> (Text, Normal) -> Repl ()
    each env (x, n) =
      pp $ DDef x (reify (Map.keys env) n)

-- | Load definitions from a file.
cmdLoad :: String -> Repl ()
cmdLoad line = mapM_ load (words line)
  where
    load :: String -> Repl ()
    load path = do
      -- TODO: catch execptions when reading.
      text <- liftIO (readFile path)
      decls <- unwrap $ parseFile path text
      mapM_ each decls

    each :: Decl -> Repl ()
    each (DDef x e) = do
      env <- get
      modify (Map.insert x (eval env e))


-- | Handle a line of input from the repl.
command :: String -> Repl ()
command line =
  R.dontCrash $ do
    s <- unwrap $ parseToplevel "repl" line
    case s of
      Right expr -> do
        env <- get
        pp $ normalize env expr
      Left (DDef x e) -> do
        env <- get
        let n = eval env e
        modify (Map.insert x n)
        pp $ DDef x (reify (Map.keys env) n)

-- | Return auto-complete suggestions for a given identifier.
complete :: String -> StateT Env IO [String]
complete s = do
  env <- get
  let names = filter (T.isPrefixOf $ T.pack s) (Map.keys env)
  return $ map T.unpack names

repl :: IO ()
repl = flip evalStateT Map.empty $ R.evalReplOpts $ R.ReplOpts
  { R.banner           = banner
  , R.command          = command
  , R.options          = options
  , R.prefix           = Just ':'
  , R.multilineCommand = Just "paste"
  , R.tabComplete      = R.Word0 complete
  , R.initialiser      = hello
  , R.finaliser        = return R.Exit
  }

-- ---------------------------------------------------------------------------
-- Miscellaneous

-- | Pretty print a value.
pp :: Pretty a => a -> Repl ()
pp x = liftIO $ do
  P.putDoc (P.pretty x)
  putStrLn ""

-- | Unwrap an 'Either'. Abort if 'Left'.
unwrap :: Show a => Either a b -> Repl b
unwrap (Right x) = return x
unwrap (Left err) = liftIO (print err) >> R.abort
