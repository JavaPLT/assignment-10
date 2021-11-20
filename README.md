Due: December 6, 11:59 PM

## General Instructions

### You Must

1. Define the functions left as `undefined` in `src/Simplifier.hs`.
2. Add `evalTests` and `toBoolTests` in `test/Spec.hs`. Additionally, add more tests in `test/Spec.hs` as you see fit. Run `stack test` to make sure that your code passes the tests.
3. Annotate all your top level definitions with types.

### You Must Not

1. Do not alter the directory structure of the project, or change the file names or module names. Please do not alter the given files other than `src/Simplifier.hs` and `test/Spec.hs`. 
2. Do not rename or change the type of any function that has already been provided to you.

### You May

1. You are encouraged to use the combinators in the `Prelude` (i.e, the built-in functions). Importing libraries included in `base` is allowed, but generally unhelpful for this assignment.
2. As before, `stack ghci` is still your friend. You can `import Parser`, `import UnParser`, and `import Simplifier` to get started. See `test/Spec.hs` for some examples on how to use the included parsers in order to test your code using `ghci` as you go.
3. In the process of your development, you may, by mistake, include an infinite loop which builds a very large expression in memory. Haskell programs can be very fast and this could very quickly consume a lot of memory freezing your computer. This is sometimes a frustrating part of the development experience. If everything is done right, however, the simplifier should be able to simplify the expression in `data/bigData1` in under a second.


## The Problem

Here you will be reimplementing the Boolean Simplifier in Haskell. Please see the description of Homework 5 for a thorough description of the relevant algorithm.

We will try to encode properties of the data at every step of the transformation into the type signature as much as possible. To do so, we have broken down the expressions carefully in the `Types.hs` file. In `Types.hs`, we have the type of `SimpleExpr` which is either just a literal, i.e, `True` or `False`, or an identifier. Then, we have `BoolExpr` which consists of `SimpleExpr` connected via `BAnd`, `BOr`, `BImplies`, `BNot` or `BIf`. We have `IfExpr` which is either a `SimpleExpr` or an `IIf` which represents an if expression whose subexpressions are if expressions. There is also a notion `NExpr` (normalized expression) which do not allow any if expressions in the test position.

The simplifier itself consists of several parts, defined in `Simplifier.hs`. You need to fill in the missing parts of this file. These are the main steps of the simplification

- `toIf :: BoolExpr -> IfExpr` : Referred to as `convertToIf` in HW05
- `normalize :: IfExpr -> NExpr`
- `eval :: Env -> NExpr -> NExpr` : Here `type Env = [(String, Bool)]`. You may want to use the function `lookup` in the Haskell Prelude.
- `toBool :: NExpr -> BoolExpr` : Referred to as `convertToBool` in HW05

Finally, the function `reduce :: BoolExpr -> BoolExpr` is the composition of all of these. You also need to implement the auxilary function `headNormalize :: NExpr -> NExpr -> NExpr -> NExpr`.