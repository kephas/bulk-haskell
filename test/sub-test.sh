#!/bin/sh

cabal test tests --test-option --match="$1"
