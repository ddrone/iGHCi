module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Data.Text (Text)
import Data.Text.Encoding
import Data.Sequence (Seq, (<|), (|>))
import System.Directory
import System.Process
import System.IO
import System.INotify
import System.FilePath
import Data.IORef

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Sequence as Seq

betterBreak :: Text -> Text -> Maybe (Text, Text)
betterBreak needle haystack =
  let (r1, r2) = Text.breakOn needle haystack in
  if needle `Text.isPrefixOf` r2
  then Just (r1, Text.drop (Text.length needle) r2)
  else Nothing

data Loop
  = Continue
  | Halt

untilM :: Monad m => m Loop -> m ()
untilM body = loop
  where
    loop = do
      result <- body
      case result of
        Continue -> loop
        Halt -> pure ()

guessCommand :: IO String
guessCommand = do
  wd <- getCurrentDirectory
  isStack <- doesFileExist (wd </> "stack.yaml")
  -- TODO Support Cabal
  if isStack
    then pure "stack repl"
    else pure "ghci"

main :: IO ()
main = do
  cmd <- guessCommand
  putStrLn $ concat ["Using \"", cmd, "\" as REPL command"]
  env <- initEnvironment cmd
  (writerID, readerID) <- ghciWrapper env
  untilM $ do
    Text.putStr "> "
    hFlush stdout
    line <- Text.getLine
    let command = parseCommand line
    handleCommand env command
  killThread writerID
  killThread readerID

ghciProcess :: Environment -> CreateProcess
ghciProcess env =
  (shell (ghciCommand env)) { std_in = CreatePipe, std_out = CreatePipe }

data Environment = Environment
  -- Send strings to inChannel to send commands to GHCi
  { inChannel :: Chan Text
  -- Receive strings from outChannel to get results from GHCi
  , outChannel :: Chan Text
  -- The prompt to be set to GHCi
  , prompt :: Text
  , ghciCommand :: String
  -- History of entered commands
  , replHistory :: IORef (Seq Text)
  , iNotify :: INotify
  }

ghciWriter :: Environment -> Handle -> IO ()
ghciWriter env hIn = forever $ do
  command <- readChan (inChannel env)
  Text.hPutStrLn hIn command
  hFlush hIn

ghciReader :: Environment -> Handle -> IO ()
ghciReader env hOut = loop ""
  where
    loop prefix = case betterBreak (prompt env) prefix of
      Nothing -> do
        next <- Text.hGetChunk hOut
        loop (prefix <> next)
      Just (first, rest) -> do
        writeChan (outChannel env) first
        loop rest

initEnvironment :: String -> IO Environment
initEnvironment cmd = do
  iNotify <- initINotify
  ch1 <- newChan
  ch2 <- newChan
  history <- newIORef Seq.empty
  -- TODO: generate random prompt instead of using a fixed string
  pure (Environment ch1 ch2 "secret-prompt>" cmd history iNotify)

ghciInteract :: Environment -> Text -> IO Text
ghciInteract env input = do
  writeChan (inChannel env) input
  readChan (outChannel env)

ghciWrapper :: Environment -> IO (ThreadId, ThreadId)
ghciWrapper env = do
  (Just hIn, Just hOut, _, _) <- createProcess (ghciProcess env)
  writerID <- forkIO (ghciWriter env hIn)
  readerID <- forkIO (ghciReader env hOut)
  ghciInteract env (Text.concat [":set prompt \"", prompt env, "\""])
  pure (writerID, readerID)

data Command
  = LoadFile Text
  | BadCommand Text
  | Ghci Text
  | Clear
  | Quit
  deriving (Show)

singleCommand :: Text -> (Text -> a) -> Text -> Maybe a
singleCommand prefix cb cmd =
  if prefix `Text.isPrefixOf` cmd
  then Just (cb (Text.drop (Text.length prefix) cmd))
  else Nothing

cond :: [a -> Maybe b] -> b -> a -> b
cond cases def input = go cases
  where
    go ls = case ls of
      [] -> def
      (f : fs) -> case f input of
        Nothing -> go fs
        Just y -> y

parseCond :: (Text -> a) -> [(Text, Text -> a)] -> Text -> a
parseCond def cases input =
  cond (uncurry singleCommand <$> cases) (def input) input

parseCommand :: Text -> Command
parseCommand =
  parseCond Ghci
    [ (":load ", LoadFile . Text.strip)
    , (":clear", const Clear)
    , (":quit", const Quit)
    , (":", BadCommand)
    ]

performReload :: Environment -> IO ()
performReload env = do
  putStr "\x1b[H\x1b[J"
  putStrLn "Files updated, reloading"
  Text.putStrLn =<< ghciInteract env ":reload"
  let runCommand cmd = do
        Text.putStrLn ("> " <> cmd)
        Text.putStrLn =<< ghciInteract env cmd
  mapM_ runCommand =<< readIORef (replHistory env)
  putStr "> "
  hFlush stdout

handleCommand :: Environment -> Command -> IO Loop
handleCommand env = \case
  LoadFile src -> do
    isFile <- doesFileExist (Text.unpack src)
    if isFile
      then do
        ghciInteract env (":load " <> src)
        Text.putStrLn ("Watching file " <> src)
        addWatch (iNotify env) [Close] (encodeUtf8 src) $ \event -> do
          if wasWriteable event
            then performReload env
            else pure ()
        pure Continue
      else do
        Text.putStrLn ("File does not exist: " <> src)
        pure Continue
  BadCommand cmd -> do
    putStrLn "Passing colon commands to GHCi is not supported."
    pure Continue
  Ghci text -> do
    Text.putStr =<< ghciInteract env text
    if Text.null text
      then pure Continue
      else do
        modifyIORef (replHistory env) (|> text)
        pure Continue
  Clear -> do
    writeIORef (replHistory env) Seq.empty
    pure Continue
  Quit -> do
    pure Halt

