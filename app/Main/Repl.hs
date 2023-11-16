module Main.Repl (repl) where

import Control.Monad.Trans
import STT.Eval
import STT.Parser
import STT.Syntax
import STT.Pretty
import qualified System.Console.Repline as R
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P
import Text.Printf (printf)

type Repl a = R.HaskelineT IO a

-- | The Repl prompt.
banner :: R.MultiLine -> Repl String
banner R.SingleLine = return "stt> "
banner R.MultiLine  = return "...> "

-- | Print the welcome message
hello :: Repl ()
hello = liftIO $ putStr $ unlines
  [ "Welcome to stt!"
  , "Type :help for more information."
  ]

-- | Repl meta commands.
options :: [(String, String -> Repl ())]
options =
  [ ("ast",  cmdAst)
  , ("help", cmdHelp)
  , ("quit", const R.abort)
  ]

-- | Print the list of options
cmdHelp :: String -> Repl ()
cmdHelp _ = mapM_ (\(x, y) -> liftIO $ printf "%-16s %s\n" x y) table
  where
    table :: [(String, String)]
    table =
      [ (":ast",   "Parse an expression, and print the AST")
      , (":help",  "Print this message")
      , (":paste", "Read multiple lines of input")
      , (":quit",  "Exit the REPL")
      ]

-- | Parse an expression and print the AST
cmdAst :: String -> Repl ()
cmdAst line =
  R.dontCrash $ do
    e <- unwrap $ parseExpr "repl" line
    liftIO $ print e

-- | Handle a line of input from the repl.
command :: String -> Repl ()
command line =
  R.dontCrash $ do
    e <- unwrap $ parseExpr "repl" line
    let e' = normalize [] e
    liftIO $ P.putDoc (P.pretty e')
    liftIO $ putStrLn ""

unwrap :: Show a => Either a b -> Repl b
unwrap (Right x) = return x
unwrap (Left err) = liftIO (print err) >> R.abort

-- | Return auto-complete suggestions for a given identifier.
complete :: String -> IO [String]
complete _ = return []

repl :: IO ()
repl = R.evalReplOpts $ R.ReplOpts
  { R.banner           = banner
  , R.command          = command
  , R.options          = options
  , R.prefix           = Just ':'
  , R.multilineCommand = Just "paste"
  , R.tabComplete      = R.Word0 complete
  , R.initialiser      = hello
  , R.finaliser        = return R.Exit
  }
