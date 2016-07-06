{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
--
-- > import Data.Time.LocalTime
-- >
-- > showTime :: IO Text
-- > showTime = getZonedTime >>= return . formatTimeRFC2822
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
-- > readAll = map parseTimeRFC2822 examples

module Data.Time.RFC2822 (
    -- * Basic type class
    -- $basic
    formatTimeRFC2822, parseTimeRFC2822
) where

import           Control.Applicative

import qualified Data.Attoparsec.Combinator as AC
import           Data.Attoparsec.Text
import qualified Data.Attoparsec.Text       as A
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Monoid.Textual        hiding (foldr, map)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Util


formatTimeRFC2822 :: (TextualMonoid t) => ZonedTime -> t
formatTimeRFC2822 zt@(ZonedTime lt z) = fromString (formatTime defaultTimeLocale "%a, %e %b %Y %T" zt) <> fromString printZone
  where timeZoneStr = timeZoneOffsetString z
        printZone = if timeZoneStr == timeZoneOffsetString utc
                    then " GMT"
                    else " " ++ timeZoneStr

formatsRFC2822 :: [Text]
formatsRFC2822 = do
  day  <- ["%a, ", ""]
  time <- ["%T", "%R"]   -- Support for hours:minutes
  zone <- ["GMT", "%z"]
  return $ day <> "%e %b %Y " <> time <> " " <> zone

parseTimeRFC2822 :: (TextualMonoid t) => t -> Maybe ZonedTime
parseTimeRFC2822 t = foldr (<|>) Nothing $ map parse formatsRFC2822
  where parse :: (TextualMonoid t) => t -> Maybe ZonedTime
        parse format = parseTime defaultTimeLocale (toString' format) t'

        -- t' is a trimmed t (currently only \n is trimmed)
        -- TODO: trim other white space characters
        t' :: String
        t' = lines (toString' t) >>= ("" ++)
