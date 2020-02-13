import Control.Monad (replicateM)

-- | may conflict in c#
data Console = Console
  { a      :: Int  -- ^ the first letter of the alphabet
  , static :: Int  -- ^ an integer
  }

-- | may conflict in c#
data System = System
  { return' :: Int    -- ^ not the end of the function
  , void    :: [Int]  -- ^ not nothing
  }

-- | not the main function
data Main = Main
  { int    :: System  -- ^ not an integer
  , ifTrue :: Int     -- ^ should not cause conflict
  }

keywords :: Int      -- ^ not a condition
         -> Char     -- ^ not a class
         -> String   -- ^ just a string
         -> Console  -- ^ not in
         -> [Int]    -- ^ not a loop
         -> [Main]   -- ^ contains lots of things
         -> String   -- ^ TODO
-- If this compiles, it is already a good step!
keywords if' class' i in' for words' = "TODO"

main :: IO ()
main = do
  if' <- fmap read getLine
  class' <- fmap head getLine
  i <- getLine
  in' <- readConsole
  for <- fmap (map read . words) getLine
  words' <- replicateM 2 readMain
  putStrLn $ keywords if' class' i in' for words'
  where
    readConsole = fmap ((\[a, b] -> Console (read a) (read b)) . words) getLine
    readMain = Main <$> readSystem <*> fmap read getLine
    readSystem = System <$> fmap read getLine <*> fmap (map read . words) getLine
