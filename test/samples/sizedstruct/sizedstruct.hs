import Control.Monad (replicateM)

-- | contains a list
data List = List
  { size1   :: Int    -- ^ the list's size
  , intList :: [Int]  -- ^ the integer list
  }

-- | contains a string
data String' = String'
  { size2      :: Int     -- ^ the list's size
  , stringList :: String  -- ^ the string list
  }

-- | contains a matrix
data Matrix = Matrix
  { size3    :: Int      -- ^ the list's size
  , listList :: [[Int]]  -- ^ the list list
  }

-- | this is not a 'sized struct', but a regular one!
data NotASizedStruct = NotASizedStruct
  { size4    :: Int    -- ^ not the list's size
  , intListN :: [Int]  -- ^ the integer list
  }

sizedStruct :: Int                -- ^ the size of the lists
            -> [List]             -- ^ a list of list of different sizes
            -> [String']          -- ^ a list of strings of different sizes
            -> [Matrix]           -- ^ a list of matrices of different sizes
            -> [NotASizedStruct]  -- ^ a list of list of same sizes
            -> String             -- ^ TODO
-- The is a special case.
sizedStruct n lists strings matrices same = "TODO"

main :: IO ()
main = do
  n <- fmap read getLine
  lists <- replicateM n readList
  strings <- replicateM n readString'
  matrices <- replicateM 2 readMatrix
  same <- replicateM n readNotASizedStruct
  putStrLn $ sizedStruct n lists strings matrices same
  where
    readList = List <$> fmap read getLine <*> fmap (map read . words) getLine
    readString' = String' <$> fmap read getLine <*> getLine
    readMatrix = fmap read getLine >>= \a -> Matrix a <$> (replicateM a $ fmap (map read . words) getLine)
    readNotASizedStruct = NotASizedStruct <$> fmap read getLine <*> fmap (map read . words) getLine
