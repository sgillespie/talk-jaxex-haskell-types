# Programming with Types in Haskell

The subject of this talk is taking advantage of the type system to help drive our design.

## Introduction

The goal of a static type system is to reduce runtime bugs. In particular:

 * Reject bad programs
 * Accept good programs

In order to take full advantage of the type system, our goal is to turn business logic
errors into type errors.

## Haskell Types Overview

### Syntax

In Haskell, type annotations are usually written outside the function. For example:

    id :: a -> a
    id x = x

    head :: [a] -> a
    head [] = []
    head (x:xs) = x

But we can also write them inline

    0 :: Int
    [1..] :: [Int]

### Simple Types

Haskell has all the usual simple types:

    0 :: Int
    0.1 :: Double
    'H' :: Char

### Function Types

    f :: Char -> Int
    g :: Int -> Int
    h :: Int -> Int -> Int

### Type Constructors

    a :: [Int]
    b :: Map Char Int

### Polymorphic Types

Haskell has type variables that can stand for any type

    f :: forall a. a
    g :: forall a. a -> a
    h :: forall a. [a]
    i :: forall k a. Map k a

But `forall` is usually omitted

    f :: a
    g :: a -> a
    h :: [a]
    i :: Map k a

### Typeclasses

Haskell supports function overloading with typeclasses

    class T a where
      f :: a -> a

    instance (T Int) where
      f a = a + 1

### Contexts

    f :: Show a => a -> String
    g :: Ord a => [a] -> [a]
    h :: (Ord k, Eq a) => Map k a

### User defined types

Haskell supports user-defined datatypes with type synonyms and algebraic types

    type MyString = String
    data MySum = MyInt Int | MyDouble Double (deriving Eq, Show)
    data MyProduct a = MyComposite Int Double [a]
    data MyRecord = MyRecord { a :: String, b :: Int }
    newtype Orders a = Orders { items :: [Item a] }

## Tools

### GHCi

    ghci> :t 5
    5 :: Num a => a
    ghci> :k Int
    Int :: *

### Typed Holes

    f :: Double -> Double
    f a = _ a

    <interactive>:14:35: error: [GHC-88464]
        • Found hole: _ :: Double -> Double
        • In the expression: _ a
          In an equation for ‘f’: f a = _ a
          In the instance declaration for ‘T Double’
        • Relevant bindings include
            a :: Double (bound at <interactive>:14:31)
            f :: Double -> Double (bound at <interactive>:14:29)
          Valid hole fits include
            f :: Double -> Double (bound at <interactive>:14:29)
            negate :: forall a. Num a => a -> a
              with negate @Double
              (imported from ‘Prelude’ (and originally defined in ‘GHC.Num’))
            realToFrac :: forall a b. (Real a, Fractional b) => a -> b
              with realToFrac @Double @Double
              (imported from ‘Prelude’ (and originally defined in ‘GHC.Real’))
            id :: forall a. a -> a
              with id @Double
              (imported from ‘Prelude’ (and originally defined in ‘GHC.Base’))
            pred :: forall a. Enum a => a -> a
              with pred @Double
              (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
            succ :: forall a. Enum a => a -> a
              with succ @Double
              (imported from ‘Prelude’ (and originally defined in ‘GHC.Enum’))
            (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)9

## Strategies

 * Lots of small types
 * Make illegal states unrepresentable
 * Parse, don't validate

## Guidelines

 * Break large structures down into small components
 * Use newtype wrappers to add semantic meaning to key domain types
 * Use data structures that make illegal states unrepresentable
 * Push the responsibility of failure upward as far as possible
 * Treat functions that return m () with deep suspicion

## Resources

 * https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
 * https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html
 * https://fsharpforfunandprofit.com/series/designing-with-types/
