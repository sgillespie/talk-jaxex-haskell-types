% Programming with Types in Haskell
% Sean Gillespie
% November 7, 2024

# Introduction

The goal of a static type system is to reduce runtime bugs. In particular:

 * Reject bad programs
 * Accept good programs

---

> When people say "but most business logic bugs aren't type errors", I just want to show
> them how to make bugs into type errors. -Matt Parsons

# Overview of Haskell Types

# Tools

 * GHCi
 * Typed Holes
 * Hoogle

# Strategies

 * Lots of small types
 * Make illegal states unrepresentable
 * Parse, don't validate

# Examples

# Guidelines

 * Break large structures down into small components
 * Use newtype wrappers to add semantic meaning to key domain types
 * Use data structures that make illegal states unrepresentable
 * Push the responsibility of failure upward as far as possible
 * Treat functions that return m () with deep suspicion

# Resources

 * [Parse, don't validate (Alexis King)](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
 * [Type Safety Back and Forth (Matt Parsons)](https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html)
 * [Designing with types (Scott Wlaschin)](https://fsharpforfunandprofit.com/series/designing-with-types/)
