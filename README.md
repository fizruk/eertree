# eertree

Efficient purely functional palindromic tree.

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
