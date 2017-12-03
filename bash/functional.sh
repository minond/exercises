#!/bin/bash -eu

set -o pipefail

id() {
  echo $@
}

:() {
  for x in "$@"; do
    echo "$x"
  done
}

map() {
  local fn=$1

  while read x; do
    eval "$fn $x"
  done
}

filter() {
  local fn=$1

  while read x; do
    if eval "$fn $x"; then
      echo $x
    fi
  done
}

range() {
  local start=$1
  local end=$2
  local step=${3:-1}

  while [ $start -lt $end ]; do
    echo $start
    start=$((start + step))
  done
}

head() {
  take 1
}

size() {
  local counter=0

  while read x; do
    counter=$(inc $counter)
  done

  echo $counter
}

take() {
  local num=$1

  while read x; do
    if [ $num -gt 0 ]; then
      num=$(dec $num)
      echo $x
    else
      break
    fi
  done
}

takeWhile() {
  local fn=$1

  while read x; do
    if eval "$fn $x"; then
      echo $x
    else
      break
    fi
  done
}

reduce() {
  local fn=$1
  local val=$2

  while read x; do
    val=$(eval "$fn $val $x")
  done

  echo $val
}

add() {
  local lhs=$1
  local rhs=$2
  echo $((lhs + rhs))
}

inc() {
  echo $(($1 + 1))
}

dec() {
  echo $(($1 - 1))
}

not() {
  [ $? -eq 0 ] && return 1 || return 0
}

even() {
  [ $(( $1 % 2 )) -eq 0 ]
}

odd() {
  not $(even $1)
}

len() {
  echo ${#1}
}

gt() {
  [ $1 -gt $2 ]
}

from-bin-to-sbin() {
  echo "${1/bin/sbin}"
}

as-directory() {
  echo "$1/"
}

with-readme() {
  echo "${1}README.txt"
}

file-exists() {
  [ -f "$1" ]
}

has-minimum-file-name-len() {
  gt $(len $1) 20
}

# Outputs:
# 2
# 3
# 4
# 5
# 6
: 1 2 3 4 5 | map inc

# Outputs:
# 1
# 3
# 5
# 7
# 9
# 11
# 13
range 1 15 | filter odd

# Outputs:
# 4950
range 1 100 | reduce add 0

# Outputs:
# /bin/cat
# /bin/chmod
# /bin/cp
: /bin/is-not-here /bin/cat /bin/chmod /bin/cp \
  | filter file-exists \
  | take 100 \
  | map echo

# Outputs:
# /sbin/chmod/README.txt
: /bin/is-not-here /bin/cat /bin/chmod /bin/cp \
  | filter file-exists \
  | map from-bin-to-sbin \
  | map as-directory \
  | map with-readme \
  | filter has-minimum-file-name-len \
  | map echo
