{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}

module Printing (
  Message,
  URI,
  SGRStack,
  startSGRScope,
  nestedSGRScope,
  sgrHyperlink,
  sgrHyperlinkFile,
  sgrGray,
  sgrYellow,
  sgrBlue,
  sgrRed,
  sgrPutStr,
  sgrPutStrLn,
  debug,
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
import Data.Sequence (Seq, (><))
import Data.Sequence qualified as Seq
import Data.Word (Word16)
import System.Console.ANSI (
  Color (Blue, Red, White, Yellow),
  ColorIntensity (Dull, Vivid),
  ConsoleIntensity (BoldIntensity),
  ConsoleLayer (Background, Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity),
  hGetLayerColor,
  hyperlink,
  setSGR,
 )
import System.IO (stdout)

type Message = String
type URI = String

type SGRStack = StateT (Seq SGR) IO

sgrGray :: [SGR]
sgrGray = [SetColor Foreground Dull White]

sgrYellow :: [SGR]
sgrYellow = [SetColor Foreground Dull Yellow]

sgrBlue :: [SGR]
sgrBlue = [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]

sgrRed :: [SGR]
sgrRed = [SetColor Foreground Vivid Red]

-- | Set the current graphics options using the current stack state
setSGRScope :: SGRStack ()
setSGRScope = do
  sgrs <- fmap toList get
  liftIO $ setSGR [Reset]
  liftIO $ setSGR sgrs

-- TODO: Use this and test
getCurrentDefaults :: IO (Maybe [RGB Word16])
getCurrentDefaults = do
  mBC <- hGetLayerColor stdout Background
  mFC <- hGetLayerColor stdout Foreground
  return $ sequence [mBC, mFC]

-- | Start a new SGR scope with the provided initial SGR paramaters.
startSGRScope :: [SGR] -> SGRStack a -> IO a
startSGRScope sgrs f = do
  res <- evalStateT (setSGRScope >> f) (Seq.fromList sgrs)
  setSGR [Reset]
  return res

-- | Start an inner SGR scope, appending the provided options to the stack.
-- These options are set in order, after previously provided options.
nestedSGRScope :: [SGR] -> SGRStack a -> SGRStack a
nestedSGRScope sgrs f = do
  prevSGR <- get
  res <- withStateT (>< Seq.fromList sgrs) (setSGRScope >> f)
  put prevSGR
  setSGRScope
  return res

sgrPutStr :: Message -> SGRStack ()
sgrPutStr = liftIO . putStr

sgrPutStrLn :: Message -> SGRStack ()
sgrPutStrLn msg = do
  prevSGR <- get
  sgrPutStr msg
  nestedSGRScope [Reset] $ do
    liftIO $ putStrLn mempty
  put prevSGR
  setSGRScope

sgrHyperlink :: URI -> Message -> SGRStack ()
sgrHyperlink uri msg = do
  nestedSGRScope sgrBlue (liftIO $ hyperlink uri msg)

sgrHyperlinkFile :: URI -> SGRStack ()
sgrHyperlinkFile uri = sgrHyperlink ("file://" <> uri) uri

debug :: String -> IO ()
debug msg = do
  startSGRScope [SetColor Foreground Vivid Red] $ do
    sgrPutStrLn $ unwords ["DEBUG:", msg]
