# FunStack

A pure-functional, stack-based language with a twist: the stack holds functions, not values.

## The basics

A FunStack program consists of a main function, which is composed from one or more smaller functions. The main function is applied to the program's arguments, and the resulting value is output.

For example, this program:

    Plus

with arguments `42` and `5` outputs the result `47`.

Now consider this program:

    Wrap Plus

When the program is executed, the functions `Wrap` (put a value into a single-element list) and `Plus` (add two values) are pushed to the stack. The arguments (let's use `42` and `5` again) are then passed to the topmost function on the stack, in this case `Plus`, resulting in `47`. This value is passed to the next function on the stack, `Wrap`, resulting in `[47]`. Since the stack is now empty, `[47]` is the program's output.

Functions on the stack can have constant values bound to one or more of their arguments. For instance:

    Pair 5

is a program that takes one argument and returns a two-element list containing `5` and the argument.

Functions on the stack can also be affected by modifiers. For example, the `self` modifier passes the same argument to all of a function's inputs:

    Pair self

This program takes one argument and returns a list consisting of two copies of the argument.

Some modifiers take more than one function. The `over` modifier, for instance, turns two functions into a single compound function that passes all its arguments through the second function before giving the resulting values to the first function.

    Pair Inc over

This program takes two arguments, increments each of them, and puts those two results in a list. With arguments `42` and `5`, the output is `[43,6]`.

## How to run

**Note:** Currently, the interpreter is in a very basic state. Program arguments must be passed as command-line args, and the program itself must be given on stdin. Many important builtins are not implemented yet, the type system needs more work, and some programs cause runtime errors when they should probably do something more useful.

The easiest way to compile and run the interpreter is using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/). Follow the instructions at that link to install it; then, in the top level of the FunStack repository, execute `stack build` to compile the code and `stack run` to run it. Here's an example:

    C:\Users\DLosc\FunStack>stack run 42 5
    Pair Inc over
    [43,6]

It should also be possible to compile the interpreter using [ghc](https://www.haskell.org/ghc/) directly. You will need to specify the paths to Main.hs and the other modules using the command-line option `-i./app:./lib`. (This method is currently untested.)
