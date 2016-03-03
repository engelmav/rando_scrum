import System.IO
import System.Locale
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Random

data Gongers = Joe | Bob | Frank deriving (Show, Enum, Bounded)

instance Random Gongers where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

main = do
  g <- newStdGen
  t <- getPOSIXTime :: IO POSIXTime
  let tStr = formatTime defaultTimeLocale "%F" (posixSecondsToUTCTime t)
  let victim = (randoms g :: [Gongers]) !! 0
  print victim 
  appendFile "gongers.log" (tStr ++ "\t" ++ show victim ++ "\n")
