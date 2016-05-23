#!/bin/sh

# to install aws:
# $ virtualenv venv
# $ . ./venv/bin/activate
# $ pip install awscli

. env-postgres-osx.sh
. venv/bin/activate

set -ex

# Check that we have aws
aws help > /dev/null

export STACK_YAML=stack-ghc-8.0.yaml

# Generate documentation
stack build --haddock --fast --ghc-options='+RTS -M300M -RTS' -j 2

aws s3 --profile docs.futurice.com --region eu-west-1 sync --delete \
    $(stack path --local-doc-root) \
    s3://docs.futurice.com/haskell-mega-repo
