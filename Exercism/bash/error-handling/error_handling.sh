#!/usr/bin/env bash

if [ $# -ne 1 ]; then
  echo "Usage: ./error_handling <greetee>"
  exit 1
fi

greetee=${1:-}

echo "Hello, $greetee"
