#!/usr/bin/env bash

if [ $# -lt 2 ]; then
    echo "USAGE $0 <salt-file> <template-file> [options...]"
    exit 1
fi

SALT_FILE=$1
TEMPLATE_FILE=$2
shift 2

sed -e "/CODEHERE/r$SALT_FILE" -e 's/CODEHERE//g' $TEMPLATE_FILE | lhs2TeX "$@"
