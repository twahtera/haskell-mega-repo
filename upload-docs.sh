#!/bin/sh

# to install aws:
# $ apt-get install libpython3.5-dev
# $ virtualenv --python=python3.5 venv
# $ . ./venv/bin/activate
# $ pip install awscli

if [ `uname` = "Darwin" ]; then
	. env-postgres-osx.sh
fi

. venv/bin/activate

set -ex

# Check that we have aws
aws help > /dev/null

export STACK_YAML=stack-ghc-8.0.yaml

# Generate documentation
stack build --test --no-run-tests --haddock --ghc-options='+RTS -M1G -RTS' -j 2

aws s3 --profile docs.futurice.com --region eu-west-1 sync --delete \
    $(stack path --local-doc-root) \
    s3://docs.futurice.com/haskell-mega-repo
