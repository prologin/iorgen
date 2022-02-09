import Control.Monad (replicateM)

manualFormat :: Int     -- ^ a first number
             -> Int     -- ^ a second number
             -> Int     -- ^ a third number
             -> Int     -- ^ This one on a new line
             -> [Int]   -- ^ an integer list, one per line
             -> String  -- ^ TODO
-- From the function perspective, this is just 4 integers
manualFormat a b c n onePerLine = "TODO"

main :: IO ()
main = do
  [a, b, c] <- fmap (map read . words) getLine
  n <- fmap read getLine
  onePerLine <- replicateM 3 $ fmap read getLine
  putStrLn $ manualFormat a b c n onePerLine
