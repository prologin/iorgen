-- | a char struct
data StructWithAChar = StructWithAChar
  { char1 :: Char  -- ^ a char
  , int2  :: Int   -- ^ an integer
  }

-- | a struct
data A = A
  { listInStruct   :: [Int]            -- ^ a list in a struct
  , structInStruct :: StructWithAChar  -- ^ a struct in a struct
  }

-- | a sized struct
data SizedStruct = SizedStruct
  { size           :: Int     -- ^ the size
  , stringInStruct :: String  -- ^ the string
  }

emptyLines :: [Int]        -- ^ an empty list
           -> String       -- ^ here to check correct parsing of empty line above
           -> Int          -- ^ an integer, will be 0 in the sample input
           -> [Int]        -- ^ an empty list (only in the sample)
           -> String       -- ^ an empty string
           -> String       -- ^ an other buffer string
           -> [Char]       -- ^ an empty char list
           -> [Char]       -- ^ an char list, non empty
           -> A            -- ^ a struct containing an empty line, then a struct
           -> SizedStruct  -- ^ a sized struct containing an empty line
           -> String       -- ^ a string to finish
           -> String       -- ^ TODO
-- Wow, lots of empty lines!
emptyLines emptyList bufferString n emptyInSample emptyString main' emptyCharList nonEmptyCharList structWithEmptyLine aSizedStruct finish = "TODO"

main :: IO ()
main = do
  emptyList <- fmap (map read . words) getLine
  bufferString <- getLine
  n <- fmap read getLine
  emptyInSample <- fmap (map read . words) getLine
  emptyString <- getLine
  main' <- getLine
  emptyCharList <- getLine
  nonEmptyCharList <- getLine
  structWithEmptyLine <- readA
  aSizedStruct <- readSizedStruct
  finish <- getLine
  putStrLn $ emptyLines emptyList bufferString n emptyInSample emptyString main' emptyCharList nonEmptyCharList structWithEmptyLine aSizedStruct finish
  where
    readA = A <$> fmap (map read . words) getLine <*> readStructWithAChar
    readSizedStruct = SizedStruct <$> fmap read getLine <*> getLine
    readStructWithAChar = fmap ((\[a, b] -> StructWithAChar (head a) (read b)) . words) getLine
