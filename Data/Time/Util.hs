module Data.Time.Util where

import           Control.Applicative

import           Data.Function
import           Data.Monoid         (mempty)
import           Data.Monoid.Textual hiding (foldr, map)
import           Data.Time
import           Data.Time.Format    (defaultTimeLocale)


toString' :: (TextualMonoid t) => t -> String
toString' = toString (maybe "?" (:[]) . characterPrefix)

parseTimeUsing :: (TextualMonoid t, TextualMonoid t') => [t] -> t' -> Maybe ZonedTime
parseTimeUsing formats t = foldr (<|>) Nothing $ map parse formats
    where parse :: (TextualMonoid t) => t -> Maybe ZonedTime
          parse format = parseTime defaultTimeLocale (toString' format) (toString' t)
