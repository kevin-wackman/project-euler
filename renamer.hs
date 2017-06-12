import System.Directory
--import System.FilePath.Windows
import System.IO
import Data.List


correctFileName :: FilePath -> FilePath
correctFileName ['e',n,'.','h','s'] = "e00" ++ (n:".hs")
correctFileName ['e',n,'i','n','.','t','x','t'] = "e00" ++ (n:"in.txt")
correctFileName ['e',n,m,'.','h','s'] = "e0" ++ (n:m:".hs")
correctFileName ['e',n,m,'i','n','.','t','x','t'] = "e0" ++ (n:m:"in.txt")
correctFileName n = n

main = do
    paths <- listDirectory "."
    sequence $ map (\x -> renameFile x (correctFileName x)) $ paths
    --print $ map correctFileName $ paths