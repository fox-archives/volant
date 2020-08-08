#!/bin/bash -eu

set -o pipefail

FILENAME="${FILENAME:-volantgobin}"

rm "./a.out" -f || true
rm "$FILENAME" -f || true

printf "\e[0;34m%s\e[0m\n" "go building..."
cd src
go build -o "$FILENAME"
cp "$FILENAME" ..
cd ..

printf "\e[0;34m%s\e[0m\n" "executing go binary, generating test.c"
install-pkg libgc-dev libblocksruntimee-dev || echo "SUCCESS?: ${?}"

printf "\e[0;34m%s\e[0m\n" "installing packages"
"./${FILENAME}" > test.c

printf "\e[0;34m%s\e[0m\n" "executing go generating test.c"
INC="${INC:-/usr/include/}"
clang test.c -pthread -fblocks -lBlocksRuntime -lgc -isystem$INC -v
./a.out
