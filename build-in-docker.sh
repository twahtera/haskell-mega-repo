#!/bin/sh

# This script is used to build binaries inside the docker
# See mega-repo-tool -h

set -ex

export STACK_YAML=stack-lts-6.yaml

cd /app/src

stack clean
stack build --pedantic
cp $(stack path --local-install-root)/bin/* /app/bin
