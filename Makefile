all: hask bin/read-c bin/sum-bytes-c

hask:
	stack install --local-bin-path bin

code:
	stack build hoogle intero stylish-haskell hlint

bin/%-c: %.c
	llvm-gcc -O3 -funroll-loops $< -o $@

time: all
	time head -c 20000000 </dev/urandom | bin/read-c; \
	time head -c 20000000 </dev/urandom | bin/bitstr; \

time-sum: all
	time head -c 20000000 </dev/urandom | bin/sum-bytes-c; \
	time head -c 20000000 </dev/urandom | bin/sum-bytes \

.PHONY: hask code time time-sum
