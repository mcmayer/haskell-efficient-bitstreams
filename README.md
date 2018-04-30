# Efficient Bitstreams in Haskell

See the corresponding [SO question](https://stackoverflow.com/questions/50101329/efficient-bitstreams-in-haskell).

The `Makefile` has all the various targets to run the experiments and the profiling. The default `make` will just build everything (create a `bin/` directory first!) and then `make time` will do the timing on the `longest-seq` executables. The C executables get a `-c` appended to distinguish them.

## `length`

Stream bytes and count. Source files `length.c` and `Length.hs`.

## `sum-bytes`

Stream bytes and add them up. Source files `sum-bytes.c` and `SumBytes.hs`.

## `longest-seq`

Stream bytes and find the longest sequence of identical bits. Source files `longest-seq.c` and `LongestSeq.hs`.