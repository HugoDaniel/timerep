{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |                                                                               
-- Module      : Data.Time.RFC2822
-- Copyright   : (c) 2011 Hugo Daniel Gomes
--
-- License     : BSD-style
-- Maintainer  : mr.hugo.gomes@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support for reading and displaying time in the format specified by 
-- the RFC2822 <http://www.ietf.org/rfc/rfc2822.txt> section 3.3
--
-- Example of usage:
-- >
-- > import Data.Time.LocalTime
-- >
-- > showTime :: IO String
-- > showTime = getZonedTime >>= return . showRFC2822
-- >
-- > example1 = "Fri, 21 Nov 1997 09:55:06 -0600"
-- > example2 = "Tue, 15 Nov 1994 12:45:26 GMT"
-- > example3 = "Tue, 1 Jul 2003 10:52:37 +0200"
-- > example4 = "Thu, 13 Feb 1969 23:32:54 -0330"
-- > example5 = "Mon, 24 Nov 1997 14:22:01 -0800"
-- > example6 = "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330"
-- > example7 = "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)"
-- > example8 = "24 Nov 1997 14:22:01 -0800"
-- > examples = [example1,example2,example3,example4,example5,example6,example7,example8]
-- >
-- > readAll = map readRFC2822 examples

module Data.Time.RFC2822 (
    -- * Basic type class
    -- $basic
    RFC2822(showRFC2822, readRFC2822)
) where

import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Maybe 
import System.Locale

test1  = "Fri, 21 Nov 1997 09:55:06 -0600"
test2  = "Tue, 15 Nov 1994 12:45:26 GMT"
test3  = "Tue, 1 Jul 2003 10:52:37 +0200"
test4  = "Thu, 13 Feb 1969 23:32:54 -0330"
test5  = "Mon, 24 Nov 1997 14:22:01 -0800"
test6  = "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330"
test7  = "Thu,          13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)"
test8  = "24 Nov 1997 14:22:01 -0800"
test9  = "15 Nov 1994 12:45:26 GMT"
test10 = "Mon,24 Nov 1997 14:22:01 -0800"
test11 = "Thu,\t13\n     Feb\n  1969\n        23:32\n     -0330 (Newfoundland Time)"
tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10
        , test11]
testParse = length (catMaybes (map readRFC2822 tests)) == length tests

-- ----------------------------------------------------------------------------
-- The RFC2822 class definition

-- | This class is here to allow future support for other data types 
-- like Data.Text or Data.ByteString if that becomes necessary
class RFC2822 a where
  showRFC2822 :: ZonedTime -> a
  readRFC2822 :: a -> Maybe ZonedTime

-- | For now there is only an instance for the String data type
instance RFC2822 String where
  showRFC2822 zt@(ZonedTime lt z) = 
    formatTime defaultTimeLocale "%a, %e %b %Y %T" zt ++ printZone
    where
      timeZoneStr = timeZoneOffsetString z
      printZone = if timeZoneStr == timeZoneOffsetString utc
                    then " GMT"
                    else " " ++ timeZoneStr

  readRFC2822 t = foldr (tryP t') Nothing [ p "%a, %e %b %Y %T GMT"
                                          , p "%a, %e %b %Y %T %z"
                                          , p "%e %b %Y %T GMT"
                                          , p "%e %b %Y %T %z"
                                          -- , p "%FT%T%QZ"
                                          ]
    where 
      p :: String -> String -> Maybe ZonedTime
      p f s = parseTime defaultTimeLocale f s

      tryP :: String -> (String -> Maybe a) -> Maybe a -> Maybe a
      tryP s f acc | isJust acc = acc
                   | otherwise = f s

      -- t' is a trimmed t (currently only \n is trimmed)
      -- TODO: trim other white space characters 
      t' :: String
      t' = lines t >>= ("" ++)

showTime :: IO String
showTime = getZonedTime >>= return . showRFC2822
