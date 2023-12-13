module Printing (
  Message,
  URI,
  SGRStack,
  getCurrentDefaults,
  startSGRScope,
  startSGRScopeCode,
  nestedSGRScope,
  nestedSGRScopeCode,
  sgrHyperlink,
  sgrHyperlinkCode,
  sgrHyperlinkFile,
  sgrHyperlinkFileCode,
  sgrGray,
  sgrYellow,
  sgrBlue,
  sgrRed,
  sgrPutStr,
  sgrPutStrLn,
  debug,
  debugCode,
) where

import Control.Monad.State.Strict (
  MonadIO (liftIO),
  MonadState (get, put),
  StateT,
  evalStateT,
  withStateT,
 )
import Data.Colour.RGBSpace (RGB)
import Data.Foldable (toList)
import Data.Word (Word16)
import System.Console.ANSI (
  Color (Blue, Red, White, Yellow),
  ColorIntensity (Dull, Vivid),
  ConsoleIntensity (BoldIntensity),
  ConsoleLayer (Background, Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity),
  hGetLayerColor,
  hyperlink,
  hyperlinkCode,
  setSGR,
  setSGRCode,
 )
import System.IO (stdout)

type Message = String
type URI = String

type SGRStack m a = StateT [SGR] m a

sgrGray :: [SGR]
sgrGray = [SetColor Foreground Dull White]

sgrYellow :: [SGR]
sgrYellow = [SetColor Foreground Dull Yellow]

sgrBlue :: [SGR]
sgrBlue = [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]

sgrRed :: [SGR]
sgrRed = [SetColor Foreground Vivid Red]

-- | Set the current graphics options using the current stack state
setSGRScope :: MonadIO m => SGRStack m ()
setSGRScope = do
  sgrs <- toList <$> get
  liftIO $ setSGR [Reset]
  liftIO $ setSGR sgrs

setSGRScopeCode :: Monad m => SGRStack m String
setSGRScopeCode = do
  sgrs <- toList <$> get
  return $ setSGRCode [Reset] <> setSGRCode sgrs

-- TODO: Use this and test
getCurrentDefaults :: MonadIO m => m (Maybe [RGB Word16])
getCurrentDefaults = do
  mBC <- liftIO $ hGetLayerColor stdout Background
  mFC <- liftIO $ hGetLayerColor stdout Foreground
  return $ sequence [mBC, mFC]

-- | Start a new SGR scope with the provided initial SGR parameters.
startSGRScope :: MonadIO m => [SGR] -> SGRStack m a -> m a
startSGRScope sgrs f = do
  res <- evalStateT (setSGRScope >> f) sgrs
  liftIO $ setSGR [Reset]
  return res

-- | Start a new SGR scope with the provided initial SGR parameters.
startSGRScopeCode :: Monad m => [SGR] -> SGRStack m String -> m String
startSGRScopeCode sgrs f = do
  res <- evalStateT (setSGRScopeCode >> f) sgrs
  return $ setSGRCode [Reset] <> res

-- | Start an inner SGR scope, appending the provided options to the stack.
-- These options are set in order, after previously provided options.
nestedSGRScope :: MonadIO m => [SGR] -> SGRStack m a -> SGRStack m a
nestedSGRScope sgrs f = do
  prevSGR <- get
  res <- withStateT (<> sgrs) (setSGRScope >> f)
  put prevSGR
  setSGRScope
  return res

nestedSGRScopeCode :: Monad m => [SGR] -> SGRStack m String -> SGRStack m String
nestedSGRScopeCode sgrs f = do
  prevSGR <- get
  res <- withStateT (<> sgrs) (setSGRScopeCode >> f)
  put prevSGR
  set <- setSGRScopeCode
  return $ res <> set

sgrPutStr :: MonadIO m => Message -> SGRStack m ()
sgrPutStr = liftIO . putStr

sgrPutStrLn :: MonadIO m => Message -> SGRStack m ()
sgrPutStrLn msg = do
  prevSGR <- get
  sgrPutStr msg
  nestedSGRScope [Reset] $ do
    liftIO $ putStrLn mempty
  put prevSGR
  setSGRScope

sgrHyperlink :: MonadIO m => URI -> Message -> SGRStack m ()
sgrHyperlink uri msg = do
  nestedSGRScope sgrBlue (liftIO $ hyperlink uri msg)

sgrHyperlinkCode :: Monad m => String -> String -> SGRStack m String
sgrHyperlinkCode uri msg = do
  nestedSGRScopeCode sgrBlue (return $ hyperlinkCode uri msg)

sgrHyperlinkFile :: MonadIO m => URI -> SGRStack m ()
sgrHyperlinkFile uri = sgrHyperlink ("file://" <> uri) uri

sgrHyperlinkFileCode :: Monad m => String -> SGRStack m String
sgrHyperlinkFileCode uri = sgrHyperlinkCode ("file://" <> uri) uri

debug :: MonadIO m => String -> m ()
debug msg = do
  startSGRScope [SetColor Foreground Vivid Red] $ do
    sgrPutStrLn $ unwords ["DEBUG:", msg]

debugCode :: Monad m => String -> m String
debugCode msg = do
  startSGRScopeCode [SetColor Foreground Vivid Red] $ do
    return $ unwords ["DEBUG:", msg]
