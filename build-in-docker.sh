#!/bin/sh

# This script is used to build binaries inside the docker
# See mega-repo-tool -h

set -ex

export STACK_YAML=stack-lts-6.yaml

cd /app/src

# We start with clean working dir
rm -rf .stack-work/
stack build --pedantic --allow-different-user
cp $(stack path --local-install-root)/bin/* /app/bin

# write current git hash, so we know where we are
GITHASH=$(git log --pretty=format:'%h' -n 1)
echo $GITHASH > /app/bin/git-hash.txt
