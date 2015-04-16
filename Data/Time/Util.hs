module Data.Time.Util where

import           Data.Function
import           Data.Monoid.Textual

toString :: (TextualMonoid t) => t -> String
toString = flip fix mempty $ \recurse output input -> case splitCharacterPrefix input of
  Just (char, suffix) -> recurse (output ++ [char]) suffix
  _ -> output
