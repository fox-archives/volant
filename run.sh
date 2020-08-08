#!/bin/bash -eu

# prequisite packpackages
# libgc-dev
# libblocksruntimee-dev

set -o pipefail

cd src
go run . > test.c
cp test.c ../
cd ..

foo=/usr/include/
(clang test.c -pthread -fblocks -lBlocksRuntime -lgc -isystem$foo && clang test.c -pthread -fblocks -lBlocksRuntime -lgc -isystem${foo}) && ./a.out
