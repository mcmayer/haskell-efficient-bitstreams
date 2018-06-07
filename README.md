# Efficient Bitstreams in Haskell

See the corresponding [SO question](https://stackoverflow.com/questions/50101329/efficient-bitstreams-in-haskell).

The `Makefile` has all the various targets to run the experiments and the profiling. The default `make` will just build everything (create a `bin/` directory first!) and then `make time` will do the timing on the `longest-seq` executables. The C executables get a `-c` appended to distinguish them.

## Setup and run

```bash
mkdir bin	# for C and Haskell executables
git clone git@github.com:haskell-streaming/streaming.git	# get the newest streaming 
```

### Makefile targets

- `make time` runs all the `longest-seq*` executables with `time`.
- `make time-sum` runs all the `sum-bytes*` executables with `time`.
- `make time-length` runs all the `length*` executables with `time`.

## `length`

Stream bytes and count. Source files `length.c` and `Length.hs`.

## `sum-bytes`

Stream bytes and add them up. Source files `sum-bytes.c` and `SumBytes.hs`.

## `longest-seq`

Stream bytes and find the longest sequence of identical bits. 

- Naive implementation using `bytestring`. Source files `longest-seq.c` and `LongestSeq.hs`.
- Implementation using `streaming `. Source file is `LongestSeqStream.hs`.
- [Cirdec's](https://stackoverflow.com/users/414413/cirdec) solution using stream fusion.

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
