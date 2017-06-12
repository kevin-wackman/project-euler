import System.Directory
import System.IO
import Control.Monad


correctFileName :: FilePath -> FilePath
correctFileName ['e',n,'.','h','s'] = "e00" ++ (n:".hs")
correctFileName ['e',n,'i','n','.','t','x','t'] = "e00" ++ (n:"in.txt")
correctFileName ['e',n,m,'.','h','s'] = "e0" ++ (n:m:".hs")
correctFileName ['e',n,m,'i','n','.','t','x','t'] = "e0" ++ (n:m:"in.txt")
correctFileName n = n

main = do
    paths <- listDirectory "."
    files <- filterM doesFileExist $ paths
    sequence $ map (\x -> renameFile x (correctFileName x)) $ files
