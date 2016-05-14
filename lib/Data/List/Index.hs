{-# LANGUAGE
CPP,
ScopedTypeVariables,
BangPatterns
  #-}


module Data.List.Index
(
  -- * Folds
  ifoldl, ifoldl',
)
where


#if __GLASGOW_HASKELL__ >= 710
import GHC.Base (oneShot)
#define ONE_SHOT oneShot
#else
#define ONE_SHOT
#endif

{- Left to implement:

imap
iany
iall
iconcatMap
ifind
ifoldrM
ifoldlM
imapAccumR
imapAccumL
ifilter

itraverse
itraverse_
ifor
ifor_
imapM
imapM_
iforM
iforM_

ifoldr
ifoldr'
ifoldr1
ifoldl1
ifoldl1'

izipWith
izipWith3
izipWith4
izipWith5
izipWith6
izipWithM
izipWithM_
-}


ifoldl :: forall a b. (Int -> b -> a -> b) -> b -> [a] -> b
ifoldl k z0 xs =
  foldr (\(v::a) (fn :: (Int, b) -> b) ->
          ONE_SHOT (\((!i)::Int, z::b) -> fn (i+1, k i z v)))
                   (snd :: (Int, b) -> b)
                   xs
                   (0, z0)
{-# INLINE ifoldl #-}

ifoldl' :: forall a b . (Int -> b -> a -> b) -> b -> [a] -> b
ifoldl' k z0 xs =
  foldr (\(v::a) (fn :: (Int, b) -> b) ->
          ONE_SHOT (\((!i)::Int, z::b) -> z `seq` fn (i+1, k i z v)))
                   (snd :: (Int, b) -> b)
                   xs
                   (0, z0)
{-# INLINE ifoldl' #-}
