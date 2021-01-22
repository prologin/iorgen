import Control.Monad (replicateM)

lists :: Int         -- ^ the first list's size
      -> [Int]       -- ^ a list containing ints
      -> Int         -- ^ an other size
      -> [Char]      -- ^ a list of char
      -> String      -- ^ a string
      -> [String]    -- ^ a list of strings of size 4
      -> [[String]]  -- ^ a list of list of strings of size 2 of size 2 of size 2
      -> [[Int]]     -- ^ a matrix of int
      -> String      -- ^ TODO
-- Aren't these lists beautifull?
lists n listInt size listChar string listString4 listListString2 matrix = "TODO"

main :: IO ()
main = do
  n <- fmap read getLine
  listInt <- fmap (map read . words) getLine
  size <- fmap read getLine
  listChar <- getLine
  string <- getLine
  listString4 <- replicateM size getLine
  listListString2 <- replicateM 2 $ replicateM 2 getLine
  matrix <- replicateM size $ fmap (map read . words) getLine
  putStrLn $ lists n listInt size listChar string listString4 listListString2 matrix
