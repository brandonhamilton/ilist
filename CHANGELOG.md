# Changelog

`ilist` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.3.1.1 — Dec 26, 2019

* Support latest major GHC versions.
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
