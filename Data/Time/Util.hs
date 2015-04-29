module Data.Time.Util where

import           Control.Applicative

import           Data.Function
import           Data.Monoid.Textual hiding (foldr, map)
import           Data.Time


toString :: (TextualMonoid t) => t -> String
toString = flip fix mempty $ \recurse output input -> case splitCharacterPrefix input of
  Just (char, suffix) -> recurse (output ++ [char]) suffix
  _ -> output

parseTimeUsing :: (TextualMonoid t, TextualMonoid t') => [t] -> t' -> Maybe ZonedTime
parseTimeUsing formats t = foldr (<|>) Nothing $ map parse formats
    where parse :: (TextualMonoid t) => t -> Maybe ZonedTime
          parse format = parseTime defaultTimeLocale (toString format) (toString t)
