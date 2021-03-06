{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
--
-- > import Data.Time.LocalTime
-- >
-- > showTime :: IO Text
-- > showTime = formatTimeRFC3339 <$> getZonedTime
-- >
-- > example1 = "1985-04-12T23:20:50.52Z"
-- > example2 = "1996-12-19T16:39:57-08:00"
-- > example3 = "1990-12-31T23:59:60Z"
-- > example4 = "1990-12-31T15:59:60-08:00"
-- > example5 = "1937-01-01T12:00:27.87+00:20"
-- > examples = [example1,example2,example3,example4,example5]
-- >
-- > readAll = map parseTimeRFC3339 examples

module Data.Time.RFC3339 (
    -- * Basic type class
    -- $basic
    formatTimeRFC3339, formatDateRFC3339, parseTimeRFC3339, parseDateRFC3339
) where

import           Control.Applicative

import           Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Monoid.Textual hiding (foldr, map)
import           Data.String         (fromString)
import           Data.Text           (Text)
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Util


formatTimeRFC3339 :: (TextualMonoid t) => ZonedTime -> t
formatTimeRFC3339 zt@(ZonedTime lt z) = fromString (formatTime defaultTimeLocale "%FT%T" zt) <> fromString printZone
  where timeZoneStr = timeZoneOffsetString z
        printZone = if timeZoneStr == timeZoneOffsetString utc
                    then "Z"
                    else take 3 timeZoneStr <> ":" <> drop 3 timeZoneStr

formatDateRFC3339 :: (TextualMonoid t, FormatTime time) => time -> t
formatDateRFC3339 day = fromString (formatTime defaultTimeLocale "%F" day)

formatsRFC3339 :: [Text]
formatsRFC3339 = do
  fraction <- ["%Q", ""]
  zone <- ["Z", "%z"]
  return $ "%FT%T" <> fraction <> zone

parseTimeRFC3339 :: (TextualMonoid t) => t -> Maybe ZonedTime
parseTimeRFC3339 = parseTimeUsing formatsRFC3339

parseDateRFC3339 :: (TextualMonoid t) => t -> Maybe Day
parseDateRFC3339 = parseTimeUsing ["%F" :: Text]
