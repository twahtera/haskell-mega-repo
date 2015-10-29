# haskell-mega-repo

> Sometimes you need to make a change across the package boundaries

## Maintaining

### Tracking the master

```
git submodule foreach git checkout master
git submodule foreach git pull
stack test
```

### Rough stats

```
wc */src/**/*.hs
```
