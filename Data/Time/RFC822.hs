{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Support for reading and displaying time in the format specified by
-- the RFC822 <http://www.ietf.org/rfc/rfc0822.txt> section 5.
--
-- Example of usage:
--
-- > import Data.Time.LocalTime
-- >
-- > showTime :: IO Text
-- > showTime = formatTimeRFC822 <$> getZonedTime
-- >
-- > example1 = "Wed, 02 Oct 2002 13:00:00 GMT"
-- > example2 = "Wed, 02 Oct 2002 13:00:00 +0100"
-- > example3 = "Wed, 02 Oct 2002 13:00 +0100"
-- > example4 = "02 Oct 2002 13:00 +0100"
-- > example5 = "02 Oct 02 13:00 +0100"
-- > examples = [example1, example2, example3, example4, example5]
-- >
-- > readAll = map parseTimeRFC822 examples

module Data.Time.RFC822 (
    -- * Basic type class
    -- $basic
    formatTimeRFC822, parseTimeRFC822
) where

import           Control.Applicative

import           Data.Maybe
import           Data.Monoid.Textual hiding (foldr, map)
import           Data.String         (fromString)
import           Data.Text           (Text)
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Util


formatTimeRFC822 :: (TextualMonoid t) => ZonedTime -> t
formatTimeRFC822 zonedTime = fromString $ formatTime defaultTimeLocale "%a, %d %b %Y %X %z" zonedTime

formatsRFC822 :: [Text]
formatsRFC822 = [ "%a, %e %b %y %X %z"
                , "%a, %e %b %y %X %Z"
                , "%a, %e %b %y %H:%M %z"
                , "%a, %e %b %y %H:%M %Z"
                , "%a, %e %b %Y %X %z"
                , "%a, %e %b %Y %X %Z"
                , "%a, %e %b %Y %H:%M %z"
                , "%a, %e %b %Y %H:%M %Z"
                , "%e %b %y %X %z"
                , "%e %b %y %X %Z"
                , "%e %b %y %H:%M %z"
                , "%e %b %y %H:%M %Z"
                , "%e %b %Y %X %z"
                , "%e %b %Y %X %Z"
                , "%e %b %Y %H:%M %z"
                , "%e %b %Y %H:%M %Z"
                ]

parseTimeRFC822 :: (TextualMonoid t) => t -> Maybe ZonedTime
parseTimeRFC822 = parseTimeUsing formatsRFC822
