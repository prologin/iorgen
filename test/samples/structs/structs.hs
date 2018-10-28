import Control.Applicative ((<$>), (<*>))
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

structs :: Struct1    -- ^ a struct 1 instance
        -> Int        -- ^ a number
        -> [Struct1]  -- ^ a list a struct 1
        -> [Point]    -- ^ a triangle
        -> String     -- ^ TODO
-- Look at them structs.
structs struct n structList triangle = "TODO"

main :: IO ()
main = do
  struct <- readStruct1
  n <- fmap read getLine
  structList <- replicateM n readStruct1
  triangle <- replicateM 3 readPoint
  putStrLn $ structs struct n structList triangle
  where
    readStruct1 = fmap ((\[a, b] -> Struct1 (read a) (read b)) . words) getLine
    readPoint = Point <$> fmap head getLine <*> readPosition
    readPosition = fmap ((\[a, b, c] -> Position (read a) (read b) (read c)) . words) getLine
