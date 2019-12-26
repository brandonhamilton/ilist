# Changelog

`ilist` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.4.0.0 — Dec 26, 2019

* Support GHC-8.8, GHC-8.6, GHC-8.4, GHC-8.2. Drop older GHC versions support.
* Update maintenance info.

## 0.3.1.0

* Added `ireplicateM` and `ireplicateM_`.

## 0.3.0.0

* `ifind` now returns the index alongside with the value (same as in `lens`).

## 0.2.0.0

* `izipWithM` and `izipWithM_` have been generalised from `Monad` to `Applicative` (which mimics what was done in base-4.9).

## 0.1.0.0

First release.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/ilist/releases
