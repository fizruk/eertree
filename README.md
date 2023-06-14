# eertree

Efficient purely functional palindromic tree.

[![Build status](https://github.com/fizruk/eertree/actions/workflows/ci.yml/badge.svg)](https://github.com/fizruk/eertree/actions/workflows/ci.yml)
[![Haddock](https://github.com/fizruk/eertree/actions/workflows/haddock.yml/badge.svg)](https://fizruk.github.io/eertree/)

_Inspired by «EERTREE: An Efficient Data Structure for Processing Palindromes in Strings» by Mikhail Rubinchik and Arseny M. Shur. See <https://arxiv.org/pdf/1506.04862v2.pdf> for more details._

## Counting rich binary strings

```
stack build && stack exec a216264
```

```
How many elements of A216264 to compute?
n = 25
1, 2, 4, 8, 16, 32, 64, 128, 252, 488, 932, 1756, 3246, 5916, 10618, 18800, 32846, 56704, 96702, 163184, 272460, 450586, 738274, 1199376, 1932338
```

## Development

### Build

To build this project simply run

```sh
stack build
```

This should install all dependencies,
including a proper version of GHC if needed.

### Benchmarks
To run C++ benchmarks:
Go to bench/cpp directory and run
```sh
make run
```
Alternatively, you can use 
```sh
make clean
make build
```
to clean and build only.
The first two benchmark runs represent the time taken by two different C++ implementations to build a tree of size N and get all subpalindromes from the eertree.
The second two benchmark runs represent the time taken by two different C++ implementations to calculate the number of rich strings of size N by utilizing eertrees.