all: bin bin/longest-seq-c bin/sum-bytes-c bin/length-c install

CC=clang-5.0

install:
	stack install --local-bin-path bin

bin/%-c: %.c
	$(CC) -O3 -funroll-loops $< -o $@

time: all
	echo "\nlongest-seq-c:" && time head -c 10000000 </dev/urandom | bin/longest-seq-c; \
	echo "\nlongest-seq:" && time head -c 10000000 </dev/urandom | bin/longest-seq; \
	echo "\nlongest-seq-stream:" && time head -c 10000000 </dev/urandom | bin/longest-seq-stream; \
	echo "\nlongest-seq-vstream:" && time head -c 10000000 </dev/urandom | bin/longest-seq-vstream; \
	echo "\nlongest-seq-vstream2:" && time head -c 10000000 </dev/urandom | bin/longest-seq-vstream2; \
	echo "\nlongest-seq-vstream3:" && time head -c 10000000 </dev/urandom | bin/longest-seq-vstream3; \
	echo "\nlongest-seq-fuse:" && time head -c 10000000 </dev/urandom | bin/longest-seq-fuse; \

time-sum: all
	time head -c 100000000 </dev/urandom | bin/sum-bytes-c; \
	time head -c 100000000 </dev/urandom | bin/sum-bytes; \
	time head -c 100000000 </dev/urandom | bin/sum-bytes-stream; \
	time head -c 100000000 </dev/urandom | bin/sum-bytes-vstream; \

time-length: all
	time head -c 100000000 </dev/urandom | bin/length-c; \
	time head -c 100000000 </dev/urandom | bin/length; \

profile: build-profile
	head -c 1000000 </dev/urandom | stack exec -- longest-seq +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- longest-seq-stream +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- longest-seq-fuse +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- sum-bytes +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- sum-bytes-stream +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- longest-seq-vstream +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- longest-seq-vstream2 +RTS -p; \
	head -c 10000 </dev/urandom | stack exec -- longest-seq-vstream3 +RTS -p; \
	head -c 1000000 </dev/urandom | stack exec -- length +RTS -p; \

build-profile:
	stack build --profile   

code:
	stack build hoogle intero stylish-haskell hlint; \
	zsh -c -i "code ."

clean:
	rm -f bin/* *.prof

bin:
	mkdir -p bin

.PHONY: install time time-sum time-length profile build-profile code clean
