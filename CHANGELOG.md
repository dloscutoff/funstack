# Changelog for `funstack`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added
- Builtins: Group
- Modifiers: arity2, arity3, filter, treeunfold
- Stack operations: !dup2, !swap2
- String interpolation syntax

### Changed
- Interpreter stops outputting and displays a warning if the output is over 10,000 characters
- Specific error messages for unterminated string, character, and list literals

## 0.0.4 - 2026-01-25

### Added
- Builtins: Max, Maximum, Min, Minimum
- Modifiers: treeapply, treemap, treewalk
- Stack operations: !box, !drop, !over, !rot, !unbox
- Aliases: Unlines (Lines)

### Fixed
- Errors caused by cycling an empty list
- Applying a modifier to not enough stack items now fills in with Id, as intended

### Changed
- Interpreter checks whether first command-line arg is a valid file before attempting to read the program from it (if not, it's treated as just another argument and the program is read from stdin)
- Cycle and related builtins, when given empty list, return empty list instead of erroring
- Flipped order of arguments for comparison builtins

## 0.0.3 - 2024-06-01

### Added
- Single-line comment syntax starting with ;
- Builtins: Read
- Modifiers: unfoldr
- Stack operations: !dup, !swap, !tuck

### Changed
- Interpreter can read a program from a file
- Scanner can handle tokens containing whitespace
- All whitespace outside of tokens is ignored, including newlines
- Scanner can (usually) handle adjacent tokens with no separating whitespace, if the separation point isn't ambiguous
- Swapped order of arguments to while and until modifiers (the condition function now comes first, matching the order of the if modifier)
- Renamed builtin: RectDepth -> UniformDepth
- Renamed special value: #N+ -> #N1

## 0.0.2 - 2022-11-02

### Added
- Builtins: Chunks, Partition
- Modifiers: mapwindows, until, while

### Changed
- Length of a number is now the length of its stringification

## 0.0.1 - 2022-10-14

### Added
- Builtins: All?, Any?, Compare, FlattenAll, Head, Init, Inits/Prefixes, Last, Not/Falsey?, Nub/Unique, Product, Repeat, Reverse, Sort, Sum, Tail, Tails/Suffixes
- Modifiers: and, branch, dropwhile, fixiter, fixpoint, flatmap, foldl, foldl1, foldr, foldr1, fork, if, invariant, not, or, pair, scanl, scanl1, scanr, scanr1, takewhile

## 0.0.0 - 2022-10-12

### Added
- Initial alpha version of FunStack interpreter
- MIT License
- Basic README
