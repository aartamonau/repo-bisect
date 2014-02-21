import Control.Monad (when)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist)
import System.FilePath (combine, takeDirectory)

repoDirName :: FilePath
repoDirName = ".repo"

findRootDir :: IO FilePath
findRootDir = go =<< canonicalizePath =<< getCurrentDirectory
  where go :: FilePath -> IO FilePath
        go dir = do
          exists <- doesDirectoryExist $ combine dir repoDirName
          if exists
            then return dir
            else do
              let parentDir = takeDirectory dir
              when (parentDir == dir) $
                error "Could not find .repo directory"
              go parentDir

main :: IO ()
main = print =<< findRootDir
