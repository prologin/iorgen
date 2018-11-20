import Control.Monad (replicateM)

-- | A struct for the example
data AStruct = AStruct
  { integer   :: Int   -- ^ an integer
  , character :: Char  -- ^ a char
  }

example :: Int        -- ^ a number, used as a size
        -> [AStruct]  -- ^ a list of structs
        -> String     -- ^ TODO
-- In a real life scenario, you will describe here what you want the end user
-- to do with this generated code
example n list = "TODO"

main :: IO ()
main = do
  n <- fmap read getLine
  list <- replicateM n readAStruct
  putStrLn $ example n list
  where
    readAStruct = fmap ((\[a, b] -> AStruct (read a) (head b)) . words) getLine
