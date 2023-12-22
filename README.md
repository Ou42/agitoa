# Walkthrough of "A Gentle Introduction to `optparse-applicative`"

- <https://prborges.com/2023/introduction-to-optparse-applicative/>

```
$ cabal --version
cabal-install version 3.10.2.0
compiled using version 3.10.2.1 of the Cabal library

$ cabal build
Resolving dependencies...
Build profile: -w ghc-9.8.1 -O1

$ cabal run exe:agitoa
Missing: FILE

Usage: agitoa [-a|--with-answers] [-d|--max-digits Int] [-s|--separation Int] 
              [-g|--group-length Int] [-o|--output-file FILE] FILE

  Arranges arithmetic problems (sums and subtractions) vertically and side by
  side

$ cabal run exe:agitoa -- -h
Warning: The package list for 'hackage.haskell.org' is 15 days old.
Run 'cabal update' to get the latest list of available packages.
Resolving dependencies...
arranger - arithmetic arranger version 0.1.0.0

Usage: agitoa [-a|--with-answers] [-d|--max-digits Int] [-s|--separation Int] 
              [-g|--group-length Int] [-o|--output-file FILE] FILE

  Arranges arithmetic problems (sums and subtractions) vertically and side by
  side

Available options:
  -h,--help                Show this help text
  -a,--with-answers        Generate problem answers
  -d,--max-digits Int      Maximum number of digits allowed in a term.
  -s,--separation Int      Number of spaces between vertically arranged
                           problems.
  -g,--group-length Int    Number of problems in each horizontal group.
  -o,--output-file FILE    Output file for the arranged problems. If not given,
                           output is written to standard output.
  FILE                     Input file with one problem per line.

An example app for
https://www.prborges.com/2023/introduction-to-optparse-applicative
```
