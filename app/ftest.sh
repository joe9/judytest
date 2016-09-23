#!/bin/bash

# ghc --make -O2 -Wall freq.hs -o freq
stack build

K=000
M=000000
FORMAT="%U %S %M"

ftest(){
    for x in i v vu vm vs j jw l; do
        echo -n "$x	$1	$2	"
        /usr/bin/time --format="$FORMAT" ../.stack-work/install/x86_64-linux/nightly-2016-09-16/8.0.1/bin/judytest-exe $x $* > /dev/null
    done
}

echo "Test various number of inserts, maxvalue of 1M"
for count in 500$K 1$M 2$M 3$M 4$M ; do
    echo "# count = $count"
    ftest $count 1$M
done

echo
echo "Test 1M number of inserts, varying maxvalue"
for maxval in 500$K 1$M 10$M 20$M 40$M; do
    echo "# maxval = $maxval"
    ftest 1$M $maxval
done
