#!/bin/bash
#
# watch - https://github.com/mikeal/watch
#

filename="$1"
watch "raco test $filename; echo '#############################'" .
