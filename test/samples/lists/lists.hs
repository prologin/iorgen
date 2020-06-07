import Control.Monad (replicateM)

lists :: Int       -- ^ the first list's size
      -> [Int]     -- ^ a list containing ints
      -> Int       -- ^ an other size
      -> [Char]    -- ^ a list of char
      -> String    -- ^ a string
      -> [String]  -- ^ a list of strings of size 4
      -> [[Int]]   -- ^ a matrix of int
      -> String    -- ^ TODO
-- Aren't these lists beautifull?
lists n listInt size listChar string listString4 matrix = "TODO"

main :: IO ()
main = do
  n <- fmap read getLine
  listInt <- fmap (map read . words) getLine
  size <- fmap read getLine
  listChar <- getLine
  string <- getLine
  listString4 <- replicateM size getLine
  matrix <- replicateM size $ fmap (map read . words) getLine
  putStrLn $ lists n listInt size listChar string listString4 matrix
