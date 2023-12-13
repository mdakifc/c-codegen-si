# c-codegen-si
A C source code generation tool for analyzing scalar interpolation optimization pass in LLVM.

# running

1. install [GHCup](https://www.haskell.org/ghcup/)
2. run `cabal build` the project root
3. run `$(cabal list-bin c-codegen-si.cabal) > /path/to/target-folder/x.c` to generate a sample