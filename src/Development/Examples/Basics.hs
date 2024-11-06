{-# LANGUAGE ExplicitForAll #-}

module Development.Examples.Basics where

import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (head, id)

-- In Haskell, type annotations are usually written outside the function
id :: a -> a
id x = x

head :: [a] -> a
head (x : xs) = x
head [] = error "Empty list"

-- But we can also write them inline
i = 0 :: Int
is = [1 ..] :: [Int]

-- * Simple Types
i' :: Int
i' = 0

d :: Double
d = 0.1

c :: Char
c = 'H'

-- * Function types
f :: Char -> Int
f = digitToInt

g :: Int -> Int
g = (+) 1

h :: Int -> Int -> Int
h = (+)

-- * Type Constructors
a :: [Int]
a = [0]

b :: Map Char Int
b = Map.empty

-- * Polymorphic Types

-- Type variables can stand for any type
f' :: forall a. a
f' = error "Uh oh!"

g' :: forall a. a -> a
g' = id

h' :: forall k a. Map k a
h' = Map.empty

-- * Typeclasses

-- Allow us to create overloaded functions
class T a where
  t :: a -> a

instance (T Int) where
  t x = x + 1

-- * User defined types

-- Type aliases
type MyString = String

-- Algebraic Data Types
data MySum
  = MyInt Int
  | MyDouble Double
  deriving (Eq, Show)

data MyProduct a = MyProduct Int Double [a]

data MyRecord a = MyRecord
  { fieldA :: String,
    fieldB :: Int,
    fieldC :: [a]
  }
  deriving (Eq, Show)

newtype IntWrapper = IntWrapper {unMyInt :: Int}
  deriving (Eq, Show)
