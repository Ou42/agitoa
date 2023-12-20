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
AppOptions {withAnswers = False, maxDigits = 4, separation = 4, probsInGroup = 5, output = StdOut, inputFile = "input.txt"}

$ cabal run exe:agitoa -- anything
Invalid argument `anything'

Usage: agitoa [-a|--with-answers] [-d|--max-digits Int] [-s|--separation Int] 
              [-g|--group-length Int] [-o|--output-file FILE]
```
