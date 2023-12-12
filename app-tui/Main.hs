{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (bracket_)
import Control.Monad (forM, forM_, void)
import Data.Functor ((<&>))
import Data.Matrix (Matrix)
import Data.Matrix qualified as M
import Data.Vector qualified as V
import Helpers.Matrix
import Menu.Drawing
import System.Console.ANSI
import System.IO

action :: IO ()
action = do
  putStrLn "Hello"
  putStrLn "World"
  void getChar

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
 where
  f True = x <&> Just
  f _ = return Nothing

withAltScreenBuffer :: IO a -> IO a
withAltScreenBuffer act = do
  echo <- hGetEcho stdout
  buff <- hGetBuffering stdin
  bracket_ pre (post echo buff) act
 where
  pre = do
    hideCursor
    hSetEcho stdout False
    hSetBuffering stdin NoBuffering
    useAlternateScreenBuffer
  post echo buff = do
    showCursor
    hSetEcho stdout echo
    hSetBuffering stdin buff
    useNormalScreenBuffer

manualPrint :: (t -> String) -> Matrix t -> IO ()
manualPrint f m = do
  forM_ (M.toLists m) $ \row -> do
    forM_ row $ \cell -> do
      putStr $ f cell
    putStrLn ""

shiftTest :: (t -> String) -> Matrix t -> IO ()
shiftTest f m = do
  forM_ [0, M.nrows m] $ \i -> do
    putStrLn $ show i ++ ":"
    manualPrint f $ shift i 0 m
    putStrLn "\n"

colourPadDemo :: IO ()
colourPadDemo = do
  let width = 50
      fg = setSGRCode [SetColor Background Dull Green, SetColor Foreground Dull Black]
      bg = setSGRCode [SetColor Background Dull Blue]
      reset = setSGRCode [Reset]
      blank = [bg ++ " "] <> replicate (width + 2) " " <> [" " ++ reset]
      twall = [bg ++ " ", fg ++ "┏"] <> replicate width "━" <> ["┓" ++ bg, " " ++ reset]
      bwall = [bg ++ " ", fg ++ "┗"] <> replicate width "━" <> ["┛" ++ bg, " " ++ reset]
      inwall = [bg ++ " ", fg ++ "┃"] <> replicate width " " <> ["┃" ++ bg, " " ++ reset]
      box =
        M.fromLists $ [blank, twall] <> replicate 7 inwall <> [bwall, blank]

  let pad = 6
  manualPrint id box
  putStrLn "\nHorizontal Padding:\n"
  manualPrint id $ horizontalPad "X" pad box
  putStrLn "\nVertical Padding:\n"
  manualPrint id $ verticalPad "X" pad box

main :: IO ()
main = do
  -- withAltScreenBuffer action
  -- let box = shape $ mkBox 4 7 (0, 0)
  colourPadDemo
  putStrLn "\nDone"