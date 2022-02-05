import Control.Monad (replicateM)

-- | A simple struct
data Struct1 = Struct1
  { foo :: Int  -- ^ a field
  , bar :: Int  -- ^ a field
  }

-- | Represents a position
data Position = Position
  { x :: Int  -- ^ X
  , y :: Int  -- ^ Y
  , z :: Int  -- ^ Z
  }

-- | A point's name and position
data Point = Point
  { name        :: Char      -- ^ the point's name (single character)
  , description :: String    -- ^ the point's description
  , pos         :: Position  -- ^ the point's position
  }

-- | a struct of chars
data Chars = Chars
  { firstChar  :: Char  -- ^ a first char
  , secondChar :: Char  -- ^ a second char
  , thirdChar  :: Char  -- ^ a third char
  }

-- | contains a big list inside
data WithList = WithList
  { int     :: Int        -- ^ int
  , bigList :: [[[Int]]]  -- ^ list nested 3 times!
  }

structs :: Struct1    -- ^ a struct 1 instance
        -> Int        -- ^ a number
        -> [Struct1]  -- ^ a list a struct 1
        -> [Point]    -- ^ a triangle
        -> Chars      -- ^ a struct of chars
        -> WithList   -- ^ the big list struct
        -> String     -- ^ TODO
-- Look at them structs.
structs struct n structList triangle structChars bigListStruct = "TODO"

main :: IO ()
main = do
  struct <- readStruct1
  n <- fmap read getLine
  structList <- replicateM n readStruct1
  triangle <- replicateM 3 readPoint
  structChars <- readChars
  bigListStruct <- readWithList
  putStrLn $ structs struct n structList triangle structChars bigListStruct
  where
    readStruct1 = fmap ((\[a, b] -> Struct1 (read a) (read b)) . words) getLine
    readPoint = Point <$> fmap head getLine <*> getLine <*> readPosition
    readChars = fmap ((\[a, b, c] -> Chars (head a) (head b) (head c)) . words) getLine
    readWithList = WithList <$> fmap read getLine <*> replicateM 2 (replicateM 2 $ fmap (map read . words) getLine)
    readPosition = fmap ((\[a, b, c] -> Position (read a) (read b) (read c)) . words) getLine
