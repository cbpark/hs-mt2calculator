hs-mt2calculator
================

This package provides a Hasekll FFI for calculating the MT2 variable.

## Build and install

You need to install the [mt2_cwrapper](https://github.com/cbpark/mt2_cwrapper) at first. Then, supply the installation directory of the `mt2_cwrapper` when executing `cabal configure`. If the directory is `/usr/local`,

```shell
cabal configure --user --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
cabal build
cabal install
```
