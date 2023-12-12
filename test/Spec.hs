import Parsing.CourseDirectory (isYear)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  arbitraryUnicodeChar,
  chooseInteger,
  listOf1,
  quickCheck,
 )

newtype ValidYear = ValidYear String deriving (Show)
newtype InvalidYear = InvalidYear String deriving (Show)

yearGen :: Gen String
yearGen = do
  let gen = chooseInteger (0, 9)
  y1 <- gen
  y2 <- gen
  y3 <- gen
  y4 <- gen
  return $ concatMap show [y1, y2, y3, y4]

instance Arbitrary ValidYear where
  arbitrary = ValidYear <$> yearGen

instance Arbitrary InvalidYear where
  arbitrary = do
    pre <- listOf1 arbitraryUnicodeChar
    year <- yearGen
    pos <- listOf1 arbitraryUnicodeChar
    return . InvalidYear $ concatMap show [pre, year, pos]

yearValidParse :: ValidYear -> Bool
yearValidParse (ValidYear year) = isYear year

yearInvalidParse :: InvalidYear -> Bool
yearInvalidParse (InvalidYear year) = not $ isYear year

main :: IO ()
main = do
  quickCheck yearValidParse
  quickCheck yearInvalidParse
