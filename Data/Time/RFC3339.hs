{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances #-}
-- |                                                                               
-- Module      : Data.Time.RFC3339
-- Copyright   : (c) 2011 Hugo Daniel Gomes
--
-- License     : BSD-style
-- Maintainer  : mr.hugo.gomes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support for reading and displaying time in the format specified by 
-- the RFC3339 <http://www.ietf.org/rfc/rfc3339.txt>
--
-- Example of usage:
-- >
-- > import Data.Time.LocalTime
-- >
-- > showTime :: IO String
-- > showTime = getZonedTime >>= return . showRFC3339
-- >
-- > example1 = "1985-04-12T23:20:50.52Z"
-- > example2 = "1996-12-19T16:39:57-08:00"
-- > example3 = "1990-12-31T23:59:60Z"
-- > example4 = "1990-12-31T15:59:60-08:00"
-- > example5 = "1937-01-01T12:00:27.87+00:20"
-- > examples = [example1,example2,example3,example4,example5]
-- >
-- > readAll = map readRFC3339 examples

module Data.Time.RFC3339 (
    -- * Basic type class
    -- $basic
    RFC3339(showRFC3339, readRFC3339)
) where

#if __GLASGOW_HASKELL__ < 710
import System.Locale
#endif

import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Maybe 

test1 = "1985-04-12T23:20:50.52Z"
test2 = "1996-12-19T16:39:57-08:00"
test3 = "1990-12-31T23:59:60Z"
test4 = "1990-12-31T15:59:60-08:00"
test5 = "1937-01-01T12:00:27.87+00:20"
tests :: [String]
tests = [test1, test2, test3, test4, test5]
testParse = length (catMaybes (map readRFC3339 tests)) == length tests

-- ----------------------------------------------------------------------------
-- The RFC3339 class definition

-- | This class is here to allow future support for other data types 
-- like Data.Text or Data.ByteString if that becomes necessary
class RFC3339 a where
  showRFC3339 :: ZonedTime -> a
  readRFC3339 :: a -> Maybe ZonedTime
  formatRFC3339 :: [a]

-- | For now there is only an instance for the String data type
instance RFC3339 String where
  showRFC3339 zt@(ZonedTime lt z) = 
    formatTime defaultTimeLocale "%FT%T" zt ++ printZone
    where
      timeZoneStr = timeZoneOffsetString z
      printZone = if timeZoneStr == timeZoneOffsetString utc
                    then "Z"
                    else take 3 timeZoneStr ++ ":" ++ drop 3 timeZoneStr
  formatRFC3339 = [ "%FT%TZ"
                  , "%FT%T%z"
                  , "%FT%T%Q%z"
                  , "%FT%T%QZ"
                  ]
  readRFC3339 t = foldr (tryP t) Nothing $ map p formatRFC3339
    where 
      p :: String -> String -> Maybe ZonedTime
      p f s = parseTime defaultTimeLocale f s

      tryP :: String -> (String -> Maybe a) -> Maybe a -> Maybe a
      tryP s f acc | isJust acc = acc
                   | otherwise = f s

showTime :: IO String
showTime = getZonedTime >>= return . showRFC3339
