#!/bin/bash -eu

set -o pipefail

cd src
go run . > test.c && (clang test.c -pthread -fblocks -lBlocksRuntime -lgc -isystem/home/runner/.apt/usr/include/ || install-pkg libgc-dev libblocksruntimee-dev && clang test.c -pthread -fblocks -lBlocksRuntime -lgc -isystem/home/runner/.apt/usr/include/) && ./a.out
