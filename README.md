# Efficient Bitstreams in Haskell

See the corresponding [SO question](https://stackoverflow.com/questions/50101329/efficient-bitstreams-in-haskell).

The `Makefile` has all the various targets to run the experiments and the profiling. The default `make` will just build everything (create a `bin/` directory first!) and then `make time` will do the timing on the `longest-seq` executables. The C executables get a `-c` appended to distinguish them.

## `length`

Stream bytes and count. Source files `length.c` and `Length.hs`.

## `sum-bytes`

Stream bytes and add them up. Source files `sum-bytes.c` and `SumBytes.hs`.

## `longest-seq`

Stream bytes and find the longest sequence of identical bits. Source files `longest-seq.c` and `LongestSeq.hs`.

## llvm

GHC 8 needs llvm 3.9 if the llvm backend is to be enabled (`-fllvm`).

On Mac OS do this to install llvm-3.9:

```bash
brew install llvm@3.9
ln -s  /usr/local/opt/llvm@3.9/bin/opt  /usr/local/bin/opt-3.9		# needed by GHC
ln -s  /usr/local/opt/llvm@3.9/bin/llc  /usr/local/bin/llc-3.9		# needed by GHC
ln -s  /usr/local/opt/llvm@3.9/bin/clang  /usr/local/bin/clang-3.9	# for compiling C
```

and point ghc to `opt-3.9` and `llc-3.9` by adding the flags  `-pgmlo opt-3.9 -pgmlc llc-3.9` to `ghc-options` of the cabal file.