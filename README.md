# haskell-mega-repo

> Sometimes you need to make a change across the package boundaries

![dependency graph](https://raw.githubusercontent.com/futurice/haskell-mega-repo/master/deps.png)

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

### Update deps graph

```
stack dot | dot -Tpng -o deps.png 
```
