# Haskell Types Basics

Haskell is an advanced functional programming language with a strong static type
system. In this particular, Haskell is a good candidate to use type driven design.

## Introduction

The goal of a static type system is to reduce runtime bugs. In particular:

 * Reject bad programs
 * Accept good programs

However, we can take this a step farther. To take full advantage of the type system, our
strategy should be to turn business logic bugs into type errors.

## Haskell Types Primer

Now we'll take a short tour of the Haskell's type system.

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

    0 :: Int
    0.1 :: Double
    'H' :: Char

### Function Types

Haskell function signatures take the form `f :: a -> b -> c`, and can be read "f is a
function that takes arguments of type b and c and returns a value of type c".

    f :: Char -> Int
    g :: Int -> Int
    h :: Int -> Int -> Int

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

### Type Synonyms

Types can be given new names with type synonyms

    type MyString = String

### Algebraic Data Types

New types can be created by combining other types.

    -- Sum
    data
      = MyInt Int
      | MyDouble Double

    -- Product
    data MyProduct = MyProdut Int Double

The set of all possible values of a sum is a set-theoretic sum (disjoint union). For
product, it's the cartesian product. Sums and products can be combined in different ways,
for example sums of sums, products of sums, and so on.

Algebraic data types are the primary tool for type driven design an Haskell.

Haskell supports a few more variants of ADTs:

    -- Records (Products with named fields)
    data MyRecord = MyRecord { a :: String, b :: Int }

    -- Zero runtime cost types (must have a single constructor and field)
    newtype Orders = Orders [Item]

### Typeclasses

Haskell supports function overloading with typeclasses

    class T a where
      f :: a -> a

    instance (T Int) where
      f a = a + 1

### Type Constraints

Types variables can be constrained by a type class. Constraints take the form
`f :: T a =>a` and can be read "f is a function taking an argument of type a, such that a
is an instance of T".

    f :: Show a => a -> String
    g :: Ord a => [a] -> [a]
    h :: (Ord k, Eq a) => Map k a

## Tools

### GHCi

GHC offers an interactive repl, GHCi, which can be used te check the type of any expression.

    ghci> :type 5
    5 :: Num a => a
    ghci> :kind Int
    Int :: *

### Typed Holes

GHC allows special placeholders (`_`) as expressions. When GHC encounters these holes, it will
generate an error message with helpful details about the context: the expected type, local bindings,
and valid hole fits.

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

 * [Parse, don't validate (Alexis King)](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
 * [Type Safety Back and Forth (Matt Parsons)](https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html)
 * [Designing with types (Scott Wlaschin)](https://fsharpforfunandprofit.com/series/designing-with-types/)
