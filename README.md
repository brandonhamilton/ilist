# ilist

[![Hackage](https://img.shields.io/hackage/v/ilist.svg)](https://hackage.haskell.org/package/ilist)
[![Build status](https://secure.travis-ci.org/aelve/ilist.svg)](https://travis-ci.org/aelve/ilist)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/ilist/blob/master/LICENSE)

## What is this

This is a library with lots of list functions that are related to indices. It has often-reinvented `replaceAt`, `deleteAt`, etc, as well as indexed variants of functions from `Data.List` (e.g. `imap`, `ifilter`, `izipWith`). It has no dependencies, builds in about a second, and works on GHC from 7.4 to 8.0; the functions are optimised and benchmarked (for instance, the `zip [0..]` idiom is usually twice as slow, and sometimes 20× as slow).

So, this library is intended to be the canonical place for index-related functions. You are encouraged to depend on this library instead of reinventing the functions, using `zip [0..]`, or using [lens](hackage.haskell.org/package/lens) when all you need is a simple `imap` or `ifoldr` (not to mention that lens variants are usually 2–10 times slower for lists).

## Why should you care

You shouldn't, actually. This is a small library, it won't change anyone's life, and if you care about speed you probably shouldn't be using lists anyway (unless you keep your fingers crossed and hope that fusion will kick in). So, consider it more of a public service announcement – “hey, just in case you need them, index-related functions live here”.

## Usage

~~~ haskell
import Data.List.Index
~~~

Unfortunately, `Data.List.Indexed` was taken by [IndexedList](http://hackage.haskell.org/package/IndexedList), which implements such exciting things as “counted lists” and “conic lists”. Nope, I'm not bitter at all. Okay, maybe a bit.
