module StringFormatters (
  surround,
  surround1,
  cleanAbsDirPathString,
  cleanRelDirPathString,
  cleanDirNameString,
  cleanAbsFilePathString,
) where

import Path (Abs, Dir, File, Path, Rel, dirname, fromAbsDir, fromAbsFile, fromRelDir)
import System.FilePath (dropTrailingPathSeparator)

surround :: String -> String -> String -> String
surround p q x = concat [p, x, q]

surround1 :: String -> String -> String
surround1 p = surround p p

cleanAbsDirPathString :: Path Abs Dir -> FilePath
cleanAbsDirPathString = dropTrailingPathSeparator . fromAbsDir

cleanRelDirPathString :: Path Rel Dir -> FilePath
cleanRelDirPathString = dropTrailingPathSeparator . fromRelDir

cleanDirNameString :: Path b Dir -> FilePath
cleanDirNameString = cleanRelDirPathString . dirname

cleanAbsFilePathString :: Path Abs File -> FilePath
cleanAbsFilePathString = dropTrailingPathSeparator . fromAbsFile
