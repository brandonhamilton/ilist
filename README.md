# ilist

[![Travis](https://img.shields.io/travis/kowainik/ilist.svg?logo=travis)](http://travis-ci.org/kowainik/ilist)
[![AppVeyor](https://ci.appveyor.com/api/projects/status/github/kowainik/ilist?branch=master&svg=true)](https://ci.appveyor.com/project/kowainik/ilist)
[![Hackage](https://img.shields.io/hackage/v/ilist.svg?logo=haskell)](https://hackage.haskell.org/package/ilist)
[![Stackage LTS](http://stackage.org/package/ilist/badge/lts)](http://stackage.org/lts/package/ilist)
[![Stackage Nightly](http://stackage.org/package/ilist/badge/nightly)](http://stackage.org/nightly/package/ilist)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/ilist/blob/master/LICENSE)

## What is this

This is a library with lots of list functions that are related to indices. It has often-reinvented `deleteAt`, `setAt`, etc, as well as indexed variants of functions from `Data.List` (e.g. `imap`, `ifilter`, `izipWith`). It has no dependencies, builds in about a second, and works on GHC from 7.4 to 8.0; the functions are [optimised](https://github.com/aelve/ilist/blob/master/lib/Data/List/Index.hs) and benchmarked (for instance, the `zip [0..]` idiom is usually twice as slow, and sometimes 20× as slow).

So, this library is intended to be the canonical place for index-related functions. You are encouraged to depend on this library instead of reinventing the functions, using `zip [0..]`, or using [lens](hackage.haskell.org/package/lens) when all you need is a simple `imap` or `ifoldr` (not to mention that lens variants are usually 2–10 times slower for lists).

## Why should you care

You shouldn't, actually. This is a small library, it won't change anyone's life, and if you care about speed you probably shouldn't be using lists anyway (unless you keep your fingers crossed and hope that fusion will kick in). So, consider it more of a public service announcement – “hey, just in case you ever need them, index-related functions live here”.

## Usage

Unfortunately, `Data.List.Indexed` was taken by [IndexedList](http://hackage.haskell.org/package/IndexedList), which implements such exciting things as “counted lists” and “conic lists”. Nope, I'm not bitter at all. Okay, maybe a bit, even tho it's completely unfair to [IndexedList](http://hackage.haskell.org/package/IndexedList). Anyway:

~~~ haskell
import Data.List.Index
~~~

And you can use functions from `Data.List` by prepending `i` to them. There's also `indexed :: [a] -> [(Int,a)]` and a family of functions for modifying the element at an index (`deleteAt`, `setAt`, `modifyAt`, `updateAt`, `insertAt`).

Watch out – `ifoldl` has the index as the *second* parameter of the function:

~~~ haskell
ifoldl :: (b -> Int -> a -> b) -> b -> [a] -> b
~~~

That's the same convention that [containers](http://hackage.haskell.org/package/containers) and [vector](http://hackage.haskell.org/package/vector) use. Other functions pass the index as the first argument, as expected.
