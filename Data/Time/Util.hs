module Data.Time.Util where

import           Control.Applicative

import           Data.Function
import           Data.Monoid         (mempty)
import           Data.Monoid.Textual hiding (foldr, map)
import           Data.Time
import           Data.Time.Format    (ParseTime, defaultTimeLocale)


toString' :: (TextualMonoid t) => t -> String
toString' = toString (maybe "?" (:[]) . characterPrefix)

parseTimeUsing :: (TextualMonoid t, TextualMonoid t', ParseTime time) => [t] -> t' -> Maybe time
parseTimeUsing formats t = foldr (<|>) Nothing $ map parse formats
    where parse format = parseTimeM True defaultTimeLocale (toString' format) (toString' t)
