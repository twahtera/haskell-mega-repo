#!/bin/sh

stack test planmill-client --test-arguments="$PLANMILL_ADMIN $PLANMILL_SIGNATURE $PLANMILL_BASEURL"
