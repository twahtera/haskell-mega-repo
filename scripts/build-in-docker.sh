#!/bin/sh

# This script is used to build binaries inside the docker
# See scripts/build.sh

set -ex

export STACK_YAML=stack-lts-5.yaml

cd /app/src

stack clean
stack build --pedantic
cp $(stack path --local-install-root)/bin/* /app/bin
