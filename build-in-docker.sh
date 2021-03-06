#!/bin/sh

# This script is used to build binaries inside the docker
# See mega-repo-tool -h

set -ex

cd /app/src

# Check that we have somewhat clean working dir
if [ ! -z "$(git status --porcelain)" ]; then
	echo "DIRTY WORKINGDIR"
	exit 1
fi

# Don't trust stack.yaml
export STACK_YAML=stack-lts-6.yaml

# Different stack root (on docker volume!)
export STACK_ROOT=/stack-root

# We DON't start with clean working dir, it takes ages otherwise.
# However we use non-default working dir, so we don't need to wipe local changes
# Note: separate checkout is preferred anyway!
WORK_DIR=.stack-work-docker
# rm -rf $WORK_DIR

# --allow-different-user is needed as we build as root inside docker
stack --no-terminal --work-dir $WORK_DIR update
stack --no-terminal --work-dir $WORK_DIR build -j2 --pedantic --allow-different-user --only-snapshot
stack --no-terminal --work-dir $WORK_DIR build -j1 --pedantic --allow-different-user
cp $(stack --work-dir $WORK_DIR path --local-install-root)/bin/* /app/bin

# write current git hash, so we know where we are
GITHASH=$(git log --pretty=format:'%h' -n 1)
echo $GITHASH > /app/bin/git-hash.txt
