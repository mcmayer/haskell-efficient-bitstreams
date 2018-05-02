all: bin/longest-seq-c bin/sum-bytes-c bin/length-c install

CC=clang-3.9

install:
	stack install --local-bin-path bin

bin/%-c: %.c
	$(CC) -O3 -funroll-loops $< -o $@

time: all
	time head -c 10000000 </dev/urandom | bin/longest-seq-c; \
	time head -c 10000000 </dev/urandom | bin/longest-seq; \
	time head -c 10000000 </dev/urandom | bin/longest-seq-stream # only 2000!; \

time-sum: all
	time head -c 100000000 </dev/urandom | bin/sum-bytes-c; \
	time head -c 100000000 </dev/urandom | bin/sum-bytes; \
	time head -c 100000000 </dev/urandom | bin/sum-bytes-stream \

time-length: all
	time head -c 100000000 </dev/urandom | bin/length-c; \
	time head -c 100000000 </dev/urandom | bin/length \

profile: build-profile
	head -c 1000000 </dev/urandom | stack exec -- longest-seq +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- longest-seq-stream +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- sum-bytes +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- sum-bytes-stream +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- length +RTS -p

build-profile:
	stack build --profile   

code:
	stack build hoogle intero stylish-haskell hlint; \
	zsh -c -i "code ."

clean:
	rm -f bin/* *.prof

.PHONY: hask code time time-sum build-profile profile clean
