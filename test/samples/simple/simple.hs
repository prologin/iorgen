simple :: Int     -- ^ the first number
       -> Int     -- ^ the second number
       -> String  -- ^ TODO
-- Just do what you want with these numbers, like sum them.
simple n otherNumber = "TODO"

main :: IO ()
main = do
  n <- fmap read getLine
  otherNumber <- fmap read getLine
  putStrLn $ simple n otherNumber
