mt2calculator
=============

This package provides a Hasekll FFI for calculating the MT2 variable.

## Build and install

Since the MT2 calculation will be performed by [Minuit2](http://root.cern.ch/root/html/ROOT__Minuit2__Minuit2Minimizer.html), you need to install the [mt2_cwrapper](https://github.com/cbpark/mt2_cwrapper) at first. Then, you should supply the installation directory of the `mt2_cwrapper` when executing `cabal configure`. If the directory is `/usr/local`,

```shell
cabal configure --user --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
cabal build
cabal install
```
