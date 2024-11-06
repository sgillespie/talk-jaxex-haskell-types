module Development.Examples.Basics where

import Data.List.NonEmpty (NonEmpty (..))

-- Head from base is a partial function
head :: [a] -> a
head (x : xs) = x
head [] = error "Empty list"

-- A safer option is to encode failure into the type
headMay :: [a] -> Maybe a
headMay (x : xs) = Just x
headMay [] = Nothing

-- An even better way is to use a non-empty list, enforced at compile time
headNonEmpty :: NonEmpty a -> a
headNonEmpty (x :| xs) = x
