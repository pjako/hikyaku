#!/usr/bin/env sh
set -euo pipefail

cc -std=c99 -O2 -o schema_gen ../schema_gen.c
./schema_gen ref.hischema ref.h

cc -std=c99 -O2 -o ref ref.c
./ref

cc -std=c99 -O2 -o ref_ext ref_ext.c
./ref_ext
