#!/bin/sh

GIT_HASH=$(git rev-parse --short HEAD)

rm -f nym

sbcl --dynamic-space-size 2048 \
     --non-interactive \
     --disable-debugger \
     --load "nym.asd" \
     --eval '(asdf:make "nym" :force t :verbose t)' \
     --quit

echo "*** Built executable for Git hash $GIT_HASH ***"
