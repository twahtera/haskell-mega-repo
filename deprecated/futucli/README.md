# github-legacy

# Short HOWTO

[Install stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)

```
stack setup
stack build
stack exec github-legacy -- :token :orgname | tee out.txt
```
