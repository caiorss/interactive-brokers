{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module API.IB.Util where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            (ByteString)
import           Data.ByteString.Lazy             (toStrict)
import           Data.ByteString.Lazy.Builder     (Builder, charUtf8,
                                                   stringUtf8, toLazyByteString)
import           Data.List                        (intersperse)
import           Data.Monoid                      (mconcat, (<>))
import           Data.Time
import           Data.Time.Zones


-- -----------------------------------------------------------------------------

boolBinary :: Bool -> Int
boolBinary False = 0
boolBinary True = 1

stringToMaybe :: String -> Maybe String
stringToMaybe s
  | null s = Nothing
  | otherwise = Just s

stringToEnum :: (Read a) => String -> Maybe a
stringToEnum s = case reads s of
  [(s',"")] -> Just s'
  _ -> Nothing

stripChars :: String -> String -> String
stripChars = filter . flip notElem

-----------------------------------------------------------------------------

when' :: (Monad m) => Bool -> (a -> m a) -> a -> m a
when' p s = if p then s else return

-----------------------------------------------------------------------------

formatSeconds :: Int -> String
formatSeconds bs
  | bs == 1 = "1 sec"
  | bs < 60 = show bs ++ " secs"
  | bs == 60 = "1 min"
  | mod bs 60 == 0 && bs < 3600 = show (div bs 60) ++ " mins"
  | bs == 3600 = "1 hour"
  | mod bs 3600 == 0 && bs < 86400 = show (div bs 3600) ++ " hours"
  | bs == 86400 = "1 day"
  | mod bs 86400 == 0 = show bs ++ " days"
  | otherwise = show bs ++ " secs"

-----------------------------------------------------------------------------

bEmpty :: Builder
bEmpty = stringUtf8 ""

bSep :: Char -> [Builder] -> Builder
bSep sep = mconcat . intersperse (charUtf8 sep)

bSep' :: Char -> [Builder] -> Builder
bSep' sep els = bSep sep els <> charUtf8 sep

bMake :: Char -> [Builder] -> ByteString
bMake sepC = toStrict . toLazyByteString . bSep' sepC

bMsg :: Char -> [Builder] -> Maybe ByteString
bMsg sepC = return . bMake sepC

bMsgConcat :: Char -> [[Builder]] -> Maybe ByteString
bMsgConcat sepC = return . bMake sepC . concat

-----------------------------------------------------------------------------


parseFixedNum :: (Read a, Num a) => Int -> Parser a
parseFixedNum n = read <$> count n digit

parseOptionalString :: ByteString -> Parser ByteString
parseOptionalString s = option "" (string s)

parseDayYYYYMMDD :: ByteString -> Parser Day
parseDayYYYYMMDD sep = do
  d <- fromGregorianValid <$>
    parseFixedNum 4 <*>
    (parseOptionalString sep *> parseFixedNum 2) <*>
    (parseOptionalString sep *> parseFixedNum 2)
  maybe (fail "") return d

parseTimeOfDayHHMMSS :: ByteString -> Parser TimeOfDay
parseTimeOfDayHHMMSS sep = do
  t <- makeTimeOfDayValid <$>
    parseFixedNum 2 <*>
    (parseOptionalString sep *> parseFixedNum 2) <*>
    (parseOptionalString sep *> parseFixedNum 2)
  maybe (fail "") return t

parseTimeOfDayHHMM :: ByteString -> Parser TimeOfDay
parseTimeOfDayHHMM sep = do
  t <- makeTimeOfDayValid <$>
    parseFixedNum 2 <*>
    (parseOptionalString sep *> parseFixedNum 2) <*>
    pure 0
  maybe (fail "") return t

-----------------------------------------------------------------------------

today :: IO UTCTime
today = (flip UTCTime 0 . utctDay) <$> getCurrentTime

zeroTimeLocal :: LocalTime
zeroTimeLocal = LocalTime (toEnum 0) (TimeOfDay 0 0 (toEnum 0))

loadTimeZones :: [String] -> IO [TZ]
loadTimeZones = mapM loadTZFromDB
