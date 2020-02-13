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
  { name :: Char      -- ^ the point's name (single character)
  , pos  :: Position  -- ^ the point's position
  }

-- | a struct of chars
data Chars = Chars
  { firstChar  :: Char  -- ^ a first char
  , secondChar :: Char  -- ^ a second char
  , thirdChar  :: Char  -- ^ a third char
  }

structs :: Struct1    -- ^ a struct 1 instance
        -> Int        -- ^ a number
        -> [Struct1]  -- ^ a list a struct 1
        -> [Point]    -- ^ a triangle
        -> Chars      -- ^ a struct of chars
        -> String     -- ^ TODO
-- Look at them structs.
structs struct n structList triangle structChars = "TODO"

main :: IO ()
main = do
  struct <- readStruct1
  n <- fmap read getLine
  structList <- replicateM n readStruct1
  triangle <- replicateM 3 readPoint
  structChars <- readChars
  putStrLn $ structs struct n structList triangle structChars
  where
    readStruct1 = fmap ((\[a, b] -> Struct1 (read a) (read b)) . words) getLine
    readPoint = Point <$> fmap head getLine <*> readPosition
    readChars = fmap ((\[a, b, c] -> Chars (head a) (head b) (head c)) . words) getLine
    readPosition = fmap ((\[a, b, c] -> Position (read a) (read b) (read c)) . words) getLine
