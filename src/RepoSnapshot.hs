import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist)
import System.FilePath (combine, takeDirectory)

repoDirName :: FilePath
repoDirName = ".repo"

findRootDir :: IO (Maybe FilePath)
findRootDir = go =<< canonicalizePath =<< getCurrentDirectory
  where go :: FilePath -> IO (Maybe FilePath)
        go dir = do
          exists <- doesDirectoryExist $ combine dir repoDirName
          if exists
            then return $ Just dir
            else maybe (return Nothing) go (parent dir)

        parent :: FilePath -> Maybe FilePath
        parent dir | dir /= parentDir = Just parentDir
                   | otherwise        = Nothing
          where parentDir = takeDirectory dir

main :: IO ()
main = print =<< findRootDir
