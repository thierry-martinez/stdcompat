#!/usr/bin/env bash
set -e
docker build -t stdcompat -f Dockerfile .
docker run -v $PWD:/stdcompat stdcompat \
  sh -c 'cd /stdcompat && ./test_all_switches.sh'
