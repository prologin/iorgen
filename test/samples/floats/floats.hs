import Control.Monad (replicateM)

-- | Represents coordinates
data Coordinates = Coordinates
  { x :: Double  -- ^ X
  , y :: Double  -- ^ Y
  , z :: Double  -- ^ Z
  }

-- | Mix of fields that go on one line
data InlinedMix = InlinedMix
  { integer :: Int     -- ^ an integer
  , char    :: Char    -- ^ a char
  , float   :: Double  -- ^ a float
  }

-- | a struct of chars
data MultilineMix = MultilineMix
  { integer2 :: Int     -- ^ an other integer
  , string   :: String  -- ^ a string of size 5
  , float2   :: Double  -- ^ an other float
  }

floats :: Double        -- ^ a float
       -> Double        -- ^ a float, greater than f
       -> Coordinates   -- ^ some coordinates
       -> Int           -- ^ a number
       -> [Double]      -- ^ a list of floats
       -> [Double]      -- ^ a list of floats
       -> [InlinedMix]  -- ^ some inlined structs
       -> MultilineMix  -- ^ a multiline struct
       -> String        -- ^ TODO
-- Parsing is often easy, reprint mode is harder
floats f g point n floatList otherList inlined multiline = "TODO"

main :: IO ()
main = do
  f <- fmap read getLine
  g <- fmap read getLine
  point <- readCoordinates
  n <- fmap read getLine
  floatList <- fmap (map read . words) getLine
  otherList <- fmap (map read . words) getLine
  inlined <- replicateM 3 readInlinedMix
  multiline <- readMultilineMix
  putStrLn $ floats f g point n floatList otherList inlined multiline
  where
    readCoordinates = fmap ((\[a, b, c] -> Coordinates (read a) (read b) (read c)) . words) getLine
    readInlinedMix = fmap ((\[a, b, c] -> InlinedMix (read a) (head b) (read c)) . words) getLine
    readMultilineMix = MultilineMix <$> fmap read getLine <*> getLine <*> fmap read getLine
