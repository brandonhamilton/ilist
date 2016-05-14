{-# LANGUAGE
CPP,
MagicHash,
ScopedTypeVariables,
BangPatterns
  #-}


module Data.List.Index
(
  -- * Folds
  ifoldl, ifoldl',
  ifoldl1, ifoldl1',

  -- * Search
  ifindIndex,
  ifindIndices,
)
where


#if __GLASGOW_HASKELL__ >= 710
import GHC.Base (oneShot)
#define ONE_SHOT oneShot
#else
#define ONE_SHOT
#endif

import Data.Maybe
import GHC.Exts

{- Left to implement:

imap
iany
iall
iconcatMap
ifoldrM
ifoldlM
imapAccumR
imapAccumL

ifind
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

ifoldl1 :: (Int -> a -> a -> a) -> [a] -> a
ifoldl1 f (x:xs) = ifoldl f x xs
ifoldl1 _ []     = errorEmptyList "ifoldl1"

ifoldl1' :: (Int -> a -> a -> a) -> [a] -> a
ifoldl1' f (x:xs) = ifoldl' f x xs
ifoldl1' _ []     = errorEmptyList "ifoldl1'"

ifindIndex :: (Int -> a -> Bool) -> [a] -> Maybe Int
ifindIndex p = listToMaybe . ifindIndices p

ifindIndices :: (Int -> a -> Bool) -> [a] -> [Int]
ifindIndices p ls = build $ \c n ->
  let go x r k | p (I# k) x = I# k `c` r (k +# 1#)
               | otherwise  = r (k +# 1#)
  in foldr go (\_ -> n) ls 0#
{-# INLINE ifindIndices #-}

errorEmptyList :: String -> a
errorEmptyList fun = error ("Data.List.Index." ++ fun ++ ": empty list")
