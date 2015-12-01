# haskell-mega-repo

> Sometimes you need to make a change across the package boundaries

![dependency graph](https://raw.githubusercontent.com/futurice/haskell-mega-repo/master/deps.png)

## Maintaining

### Tracking the master

```
git submodule foreach git checkout master
git submodule foreach git pull
STACK_YAML=stack-lts-3.yaml stack test --pedantic
```

### Rough stats

```
wc */src/**/*.hs
```

### Update deps graph

```
stack dot | dot -Tpng -o deps.png
```

### LTS version bumps

```
git submodule foreach git checkout master
vim -p $(find . -name stack-lts-3.yaml)
STACK_YAML=stack-lts-3.yaml stack test --pedantic
# Or if really want to test each package separately
export STACK_YAML=stack-lts-3.yaml
for i in favicon-app flowdock-grep; do echo $i; cd $i; stack test --pedantic; cd ..; done
# Commit each package changes...
```

## Futurice Haskell Guidelines

### Definitions

The packages can be divided into two groups: *applications* and *libraries*.
The characteristics of these groups differ, so we apply different guidelines

### Style guide

We adhere to [Johan Tibell's styleguide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md).

We use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell).
[Example `stylish-haskell.yaml`](https://github.com/futurice/haskell-servant-status/blob/master/.stylish-haskell.yaml).

Packages have to be buildable with `-Wall -Werror` with some of the supported
GHCs (currently usually *7.10.2*).

### GHC Extensions

See [ghccaniuse](http://damianfral.github.io/ghcaniuse/). Don't use extensions,
which aren't supported by three or four GHC releases, i.e. using `DataKinds`
and other type-system extensions are ok (required by e.g. `servant` anyway).

For applications, new and straight-forward extensions like `DeriveAnyClass`,
`PartialTypeSignatures`, or upcoming `ApplicativeDo`, `StrictData` or
`SignatureSections` are ok as well. However, their usage isn't an end in
itself.

Also of `TupleSections` and `RecordWildCards` are ok.

Don't use `UnicodeSyntax`.

Use `CPP` sparingly. If you can get away with compat package (e.g.
[`base-compat`](http://hackage.haskell.org/package/base-compat)).  Some
warnings with *other* GHCs are ok.

### Three release policy

> Everything should be compilable with the last three releases of GHC.

We don't apply the three release policy for the applications. It's enough that
we can compile applications with some recent GHC version (currently *7.8.4*).

For libraries we aim to support the last three GHC versions (currently:
*7.6.3*, *7.8.4* and *7.10.2*), as well the last three Stackage LTS snapshots
(currently only two: [*lts-2.22*](https://www.stackage.org/lts-2.22) and
*lts-3*).

Stackage support is not strict. For example the API of
[`servant`](http://hackage.haskell.org/package/servant) changes with every
major release, so supporting multiple releases is not worth the trouble.

GHC 7.6.3 support is not strict. As we haven't used it in production, we support
it, if it doesn't require additional work. When GHC 8.0 will be released, the
GHC part of thre three release policy will be strict(er).
