{-# LANGUAGE
CPP,
MagicHash,
ScopedTypeVariables,
BangPatterns
  #-}


{- |
Note: a lot of these functions are available for other types (in their respective packages):

  * @<http://hackage.haskell.org/package/vector/docs/Data-Vector.html Data.Vector>@ provides 'indexed' and lots of other functions beginning with “i”.

  * @<http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html Data.Map>@ and @<http://hackage.haskell.org/package/containers/docs/Data-Sequence.html Data.Sequence>@ provide similar functions, but use a different naming convention (e.g. @<http://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html#v:mapWithKey mapWithKey>@ for maps and @<http://hackage.haskell.org/package/containers/docs/Data-Sequence.html#v:foldrWithIndex foldrWithIndex>@ for sequences).

  * <http://hackage.haskell.org/package/lens lens> provides several typeclasses for indexed functions that work on maps, lists, vectors, bytestrings, and so on (in @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Indexed.html Control.Lens.Indexed>@), but unfortunately they are pretty slow for lists.
-}
module Data.List.Index
(
  -- * Original functions
  indexed,
  deleteAt,
  setAt,
  modifyAt,
  updateAt,
  insertAt,

  -- * Adapted functions from "Data.List"
  -- $adapted

  -- ** Maps
  imap,
  imapM, imapM_,
  ifor, ifor_,
  -- ** Folds
  ifoldr, ifoldl, ifoldl',
  iall, iany, iconcatMap,
  -- ** Sublists
  ifilter, ipartition,
  itakeWhile, idropWhile,
  -- ** Zipping
  izipWith,
  izipWithM, izipWithM_,
  -- ** Search
  ifind,
  ifindIndex,
  ifindIndices,

  -- * Less commonly used functions

  -- ** Zipping
  izipWith3,
  izipWith4,
  izipWith5,
  izipWith6,
  izipWith7,

  -- ** Monadic functions
  iforM, iforM_,
  itraverse, itraverse_,
  ifoldrM,
  ifoldlM,
  
  -- ** Folds
  ifoldMap,
  imapAccumR,
  imapAccumL,
)
where


#if __GLASGOW_HASKELL__ >= 710
import GHC.Base (oneShot)
#define ONE_SHOT oneShot
#else
#define ONE_SHOT
#endif

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Foldable
import Data.Maybe
import Data.Monoid
import GHC.Exts

{- Left to do:

Functions
~~~~~~~~~

alterF or something?

iscanl
iscanl'
iscanl1
iscanr
iscanr1

iiterate?

backpermute?
minIndex/maxIndex?
-}

{- |
'indexed' pairs each element with its index.

>>> indexed "hello"
[(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]

/Subject to fusion./
-}
indexed :: [a] -> [(Int, a)]
indexed xs = go 0# xs
  where
    go i (a:as) = (I# i, a) : go (i +# 1#) as
    go _ _ = []
{-# NOINLINE [1] indexed #-}

indexedFB :: ((Int, a) -> t -> t) -> a -> (Int# -> t) -> Int# -> t
indexedFB c = \x cont i -> (I# i, x) `c` cont (i +# 1#)
{-# INLINE [0] indexedFB #-}

{-# RULES
"indexed"       [~1] forall xs.    indexed xs = build (\c n -> foldr (indexedFB c) (\_ -> n) xs 0#)
"indexedList"   [1]  forall xs.    foldr (indexedFB (:)) (\_ -> []) xs 0# = indexed xs
  #-}

{- |
'deleteAt' deletes the element at an index.

If the index is negative or exceeds list length, the original list will be returned.
-}
deleteAt :: Int -> [a] -> [a]
deleteAt i ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = xs
    go n (x:xs) = x : go (n-1) xs
    go _ [] = []
{-# INLINE deleteAt #-}

{- |
'setAt' sets the element at the index.

If the index is negative or exceeds list length, the original list will be returned.
-}
setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ [] = []
{-# INLINE setAt #-}

{- |
'modifyAt' applies a function to the element at the index.

If the index is negative or exceeds list length, the original list will be returned.
-}
modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = f x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ [] = []
{-# INLINE modifyAt #-}

{- |
'updateAt' applies a function to the element at the index, and then either replaces the element or deletes it (if the function has returned 'Nothing').

If the index is negative or exceeds list length, the original list will be returned.
-}
updateAt :: Int -> (a -> Maybe a) -> [a] -> [a]
updateAt i f ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = case f x of
      Nothing -> xs
      Just x' -> x' : xs
    go n (x:xs) = x : go (n-1) xs
    go _ [] = []
{-# INLINE updateAt #-}

{- |
'insertAt' inserts an element at the given position:

@
(insertAt i x xs) !! i == x
@

If the index is negative or exceeds list length, the original list will be returned. (If the index is equal to the list length, the insertion can be carried out.)
-}
insertAt :: Int -> a -> [a] -> [a]
insertAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 xs = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ [] = []
{-# INLINE insertAt #-}

{-

David Feuer says that drop-like functions tend to have problems when implemented with folds: <http://ircbrowse.net/browse/haskell?id=22794495&timestamp=1463607633#t1463607633>. I haven't been able to observe this, but since Data.List defines drop/dropWhile/etc that don't fuse, let's do it here as well – just in case. The original version (that does fuse) is below.

-- The plan is that if it does inline, it'll be fast; and if it doesn't
-- inline, the former definition will be used and sharing will be preserved
-- (i.e. if i == 0, it won't rebuild the whole list).
deleteAtFB :: Int -> (a -> t -> t) -> a -> (Int# -> t) -> Int# -> t
deleteAtFB (I# i) c = \x r k ->
  case k ==# i of
    0# -> x `c` r (k +# 1#)
    _  -> r (k +# 1#)
{-# INLINE [0] deleteAtFB #-}

{-# RULES
"deleteAt"       [~1] forall i xs.    deleteAt i xs = build (\c n -> foldr (deleteAtFB i c) (\_ -> n) xs 0#)
"deleteAtList"   [1]  forall i xs.    foldr (deleteAtFB i (:)) (\_ -> []) xs 0# = deleteAt i xs
  #-}

-}

{- $adapted

These functions mimic their counterparts in "Data.List" – 'imap', for instance, works like 'map' but gives the index of the element to the modifying function.

Note that left folds have the index argument /after/ the accumulator argument – that's the convention adopted by containers and vector (but not lens).
-}

{- |
/Subject to fusion./
-}
imap :: (Int -> a -> b) -> [a] -> [b]
imap f ls = go 0# ls
  where
    go i (x:xs) = f (I# i) x : go (i +# 1#) xs
    go _ _ = []
{-# NOINLINE [1] imap #-}

imapFB
  :: (b -> t -> t) -> (Int -> a -> b) -> a -> (Int# -> t) -> Int# -> t
imapFB c f = \x r k -> f (I# k) x `c` r (k +# 1#)
{-# INLINE [0] imapFB #-}

{-# RULES
"imap"       [~1] forall f xs.    imap f xs = build (\c n -> foldr (imapFB c f) (\_ -> n) xs 0#)
"imapList"   [1]  forall f xs.    foldr (imapFB (:) f) (\_ -> []) xs 0# = imap f xs
  #-}

{-
Note: we don't apply the *FB transformation to 'iconcatMap' because it uses 'ifoldr' instead of 'foldr', and 'ifoldr' might get inlined itself, and rewriting 'iconcatMap' with 'foldr' instead of 'ifoldr' is annoying. So, in theory it's a small optimisation possibility (in practice I'm not so sure, given that functions with 'build' don't seem to perform worse than functions without it).
-}
iconcatMap :: (Int -> a -> [b]) -> [a] -> [b]
iconcatMap f xs = build $ \c n ->
  ifoldr (\i x b -> foldr c b (f i x)) n xs
{-# INLINE iconcatMap #-}

ifoldMap :: Monoid m => (Int -> a -> m) -> [a] -> m
ifoldMap p ls = foldr go (\_ -> mempty) ls 0#
  where go x r k = p (I# k) x <> r (k +# 1#)
{-# INLINE ifoldMap #-}

{- |
/Subject to fusion./
-}
iall :: (Int -> a -> Bool) -> [a] -> Bool
iall p ls = foldr go (\_ -> True) ls 0#
  where go x r k = p (I# k) x && r (k +# 1#)
{-# INLINE iall #-}

{- |
/Subject to fusion./
-}
iany :: (Int -> a -> Bool) -> [a] -> Bool
iany p ls = foldr go (\_ -> False) ls 0#
  where go x r k = p (I# k) x || r (k +# 1#)
{-# INLINE iany #-}

imapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM f as = ifoldr k (return []) as
  where
    k i a r = do
      x <- f i a
      xs <- r
      return (x:xs)
{-# INLINE imapM #-}

iforM :: Monad m => [a] -> (Int -> a -> m b) -> m [b]
iforM = flip imapM
{-# INLINE iforM #-}

itraverse :: Applicative m => (Int -> a -> m b) -> [a] -> m [b]
itraverse f as = ifoldr k (pure []) as
  where
    k i a r = (:) <$> f i a <*> r
{-# INLINE itraverse #-}

ifor :: Applicative m => [a] -> (Int -> a -> m b) -> m [b]
ifor = flip itraverse
{-# INLINE ifor #-}

{- |
/Subject to fusion./
-}
imapM_ :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM_ f as = ifoldr k (return ()) as
  where
    k i a r = f i a >> r
{-# INLINE imapM_ #-}

{- |
/Subject to fusion./
-}
iforM_ :: Monad m => [a] -> (Int -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}

{- |
/Subject to fusion./
-}
itraverse_ :: Applicative m => (Int -> a -> m b) -> [a] -> m ()
itraverse_ f as = ifoldr k (pure ()) as
  where
    k i a r = f i a *> r
{-# INLINE itraverse_ #-}

{- |
/Subject to fusion./
-}
ifor_ :: Applicative m => [a] -> (Int -> a -> m b) -> m ()
ifor_ = flip itraverse_
{-# INLINE ifor_ #-}

-- Using unboxed ints here doesn't seem to result in any benefit
ifoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr f z xs = foldr (\x g i -> f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr #-}

ifoldrM :: Monad m => (Int -> a -> b -> m b) -> b -> [a] -> m b
ifoldrM f z xs = ifoldr k (return z) xs
  where
    k i a r = f i a =<< r
{-# INLINE ifoldrM #-}

imapAccumR
  :: (acc -> Int -> x -> (acc, y))
  -> acc
  -> [x]
  -> (acc, [y])
imapAccumR f z xs =
  foldr (\x g i -> let (a, ys) = g (i+1)
                       (a', y) = f a i x
                   in  (a', y:ys))
        (const (z, [])) xs 0
{-# INLINE imapAccumR #-}

{-

ifoldr1 :: (Int -> a -> a -> a) -> [a] -> a
ifoldr1 f = go 0#
  where go _ [x]    = x
        go i (x:xs) = f (I# i) x (go (i +# 1#) xs)
        go _ []     = errorEmptyList "ifoldr1"
{-# INLINE [0] ifoldr1 #-}

-}

{- |
The index isn't the first argument of the function because that's the convention adopted by containers and vector (but not lens).

/Subject to fusion./
-}
ifoldl :: forall a b. (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl k z0 xs =
  foldr (\(v::a) (fn :: (Int, b) -> b) ->
          ONE_SHOT (\((!i)::Int, z::b) -> fn (i+1, k z i v)))
        (snd :: (Int, b) -> b)
        xs
        (0, z0)
{-# INLINE ifoldl #-}

{- |
/Subject to fusion./
-}
ifoldl' :: forall a b. (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl' k z0 xs =
  foldr (\(v::a) (fn :: (Int, b) -> b) ->
          ONE_SHOT (\((!i)::Int, z::b) -> z `seq` fn (i+1, k z i v)))
        (snd :: (Int, b) -> b)
        xs
        (0, z0)
{-# INLINE ifoldl' #-}

{- |
/Subject to fusion./
-}
ifoldlM :: Monad m => (b -> Int -> a -> m b) -> b -> [a] -> m b
ifoldlM f z xs = ifoldl k (return z) xs
  where
    k a i r = do a' <- a; f a' i r
{-# INLINE ifoldlM #-}

imapAccumL
  :: (acc -> Int -> x -> (acc, y))
  -> acc
  -> [x]
  -> (acc, [y])
imapAccumL f z xs =
  foldr (\(x::a) (r :: (Int,acc) -> (acc,[y])) ->
          ONE_SHOT (\((!i)::Int, s::acc) ->
            let (s', y)   = f s i x
                (s'', ys) = r (i+1, s')
            in (s'', y:ys)))
        ((\(_, a) -> (a, [])) :: (Int,acc) -> (acc,[y]))
        xs
        (0, z)
{-# INLINE imapAccumL #-}

{-

ifoldl1 :: (a -> Int -> a -> a) -> [a] -> a
ifoldl1 f (x:xs) = ifoldl f x xs
ifoldl1 _ []     = errorEmptyList "ifoldl1"

ifoldl1' :: (a -> Int -> a -> a) -> [a] -> a
ifoldl1' f (x:xs) = ifoldl' f x xs
ifoldl1' _ []     = errorEmptyList "ifoldl1'"

-}

ifilter :: (Int -> a -> Bool) -> [a] -> [a]
ifilter p ls = go 0# ls
  where
    go i (x:xs) | p (I# i) x = x : go (i +# 1#) xs
                | otherwise  = go (i +# 1#) xs
    go _ _ = []
{-# NOINLINE [1] ifilter #-}

ifilterFB
  :: (a -> t -> t) -> (Int -> a -> Bool) -> a -> (Int# -> t) -> Int# -> t
ifilterFB c p = \x r k ->
  if p (I# k) x then x `c` r (k +# 1#) else r (k +# 1#)
{-# INLINE [0] ifilterFB #-}

{-# RULES
"ifilter"       [~1] forall p xs.    ifilter p xs = build (\c n -> foldr (ifilterFB c p) (\_ -> n) xs 0#)
"ifilterList"   [1]  forall p xs.    foldr (ifilterFB (:) p) (\_ -> []) xs 0# = ifilter p xs
  #-}

itakeWhile :: (Int -> a -> Bool) -> [a] -> [a]
itakeWhile p ls = go 0# ls
  where
    go i (x:xs) | p (I# i) x = x : go (i +# 1#) xs
                | otherwise  = []
    go _ _ = []
{-# NOINLINE [1] itakeWhile #-}

itakeWhileFB
  :: (a -> t -> t) -> (Int -> a -> Bool) -> t -> a -> (Int# -> t) -> Int# -> t
itakeWhileFB c p n = \x r k ->
  if p (I# k) x then x `c` r (k +# 1#) else n
{-# INLINE [0] itakeWhileFB #-}

{-# RULES
"itakeWhile"       [~1] forall p xs.    itakeWhile p xs = build (\c n -> foldr (itakeWhileFB c p n) (\_ -> n) xs 0#)
"itakeWhileList"   [1]  forall p xs.    foldr (itakeWhileFB (:) p []) (\_ -> []) xs 0# = itakeWhile p xs
  #-}

idropWhile :: (Int -> a -> Bool) -> [a] -> [a]
idropWhile p ls = go 0# ls
  where
    go i (x:xs) | p (I# i) x = go (i +# 1#) xs
                | otherwise  = x:xs
    go _ [] = []
{-# INLINE idropWhile #-}

ipartition :: (Int -> a -> Bool) -> [a] -> ([a],[a])
ipartition p xs = ifoldr (iselect p) ([],[]) xs
{-# INLINE ipartition #-}

iselect :: (Int -> a -> Bool) -> Int -> a -> ([a], [a]) -> ([a], [a])
iselect p i x ~(ts,fs) | p i x     = (x:ts,fs)
                       | otherwise = (ts, x:fs)

ifind :: (Int -> a -> Bool) -> [a] -> Maybe a
ifind p = listToMaybe . ifilter p

ifindIndex :: (Int -> a -> Bool) -> [a] -> Maybe Int
ifindIndex p = listToMaybe . ifindIndices p

ifindIndices :: (Int -> a -> Bool) -> [a] -> [Int]
ifindIndices p ls = go 0# ls
  where
    go _ [] = []
    go i (x:xs) | p (I# i) x = I# i : go (i +# 1#) xs
                | otherwise  = go (i +# 1#) xs
{-# NOINLINE [1] ifindIndices #-}

ifindIndicesFB
  :: (Int -> t -> t) -> (Int -> a -> Bool) -> a -> (Int# -> t) -> Int# -> t
ifindIndicesFB c p = \x r k ->
  if p (I# k) x then I# k `c` r (k +# 1#) else r (k +# 1#)
{-# INLINE [0] ifindIndicesFB #-}

{-# RULES
"ifindIndices"       [~1] forall p xs.    ifindIndices p xs = build (\c n -> foldr (ifindIndicesFB c p) (\_ -> n) xs 0#)
"ifindIndicesList"   [1]  forall p xs.    foldr (ifindIndicesFB (:) p) (\_ -> []) xs 0# = ifindIndices p xs
  #-}

{-

errorEmptyList :: String -> a
errorEmptyList fun = error ("Data.List.Index." ++ fun ++ ": empty list")

-}

{- |
/Subject to fusion in the first argument./
-}
izipWith :: (Int -> a -> b -> c) -> [a] -> [b] -> [c]
izipWith fun xs ys = go 0# xs ys
  where
    go i (a:as) (b:bs) = fun (I# i) a b : go (i +# 1#) as bs
    go _ _ _ = []
{-# NOINLINE [1] izipWith #-}

izipWithFB
  :: (c -> t -> t) -> (Int -> a -> b -> c) -> a -> b -> (Int# -> t) -> Int# -> t
izipWithFB c fun = \x y cont i -> fun (I# i) x y `c` cont (i +# 1#)
{-# INLINE [0] izipWithFB #-}

{-# RULES
"izipWith"       [~1] forall f xs ys.    izipWith f xs ys = build (\c n -> foldr2 (izipWithFB c f) (\_ -> n) xs ys 0#)
"izipWithList"   [1]  forall f xs ys.    foldr2 (izipWithFB (:) f) (\_ -> []) xs ys 0# = izipWith f xs ys
  #-}

-- Copied from GHC.List

foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 k z = go
  where
        go []    _ys     = z
        go _xs   []      = z
        go (x:xs) (y:ys) = k x y (go xs ys)
{-# INLINE [0] foldr2 #-}

foldr2_left :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left _k  z _x _r []     = z
foldr2_left  k _z  x  r (y:ys) = k x y (r ys)

{-# RULES
"foldr2/left"   forall k z ys (g::forall b.(a->b->b)->b->b) .
                  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
 #-}

izipWith3
  :: (Int -> a -> b -> c -> d)
  -> [a] -> [b] -> [c] -> [d]
izipWith3 fun = go 0#
  where
    go i (a:as) (b:bs) (c:cs) =
      fun (I# i) a b c : go (i +# 1#) as bs cs
    go _ _ _ _ = []
{-# INLINE izipWith3 #-}

izipWith4
  :: (Int -> a -> b -> c -> d -> e)
  -> [a] -> [b] -> [c] -> [d] -> [e]
izipWith4 fun = go 0#
  where
    go i (a:as) (b:bs) (c:cs) (d:ds) =
      fun (I# i) a b c d : go (i +# 1#) as bs cs ds
    go _ _ _ _ _ = []
{-# INLINE izipWith4 #-}

izipWith5
  :: (Int -> a -> b -> c -> d -> e -> f)
  -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
izipWith5 fun = go 0#
  where
    go i (a:as) (b:bs) (c:cs) (d:ds) (e:es) =
      fun (I# i) a b c d e : go (i +# 1#) as bs cs ds es
    go _ _ _ _ _ _ = []
{-# INLINE izipWith5 #-}

izipWith6
  :: (Int -> a -> b -> c -> d -> e -> f -> g)
  -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
izipWith6 fun = go 0#
  where
    go i (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) =
      fun (I# i) a b c d e f : go (i +# 1#) as bs cs ds es fs
    go _ _ _ _ _ _ _ = []
{-# INLINE izipWith6 #-}

izipWith7
  :: (Int -> a -> b -> c -> d -> e -> f -> g -> h)
  -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
izipWith7 fun = go 0#
  where
    go i (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) =
      fun (I# i) a b c d e f g : go (i +# 1#) as bs cs ds es fs gs
    go _ _ _ _ _ _ _ _ = []
{-# INLINE izipWith7 #-}

izipWithM :: Applicative f => (Int -> a -> b -> f c) -> [a] -> [b] -> f [c]
izipWithM f as bs = sequenceA (izipWith f as bs)
{-# INLINE izipWithM #-}

izipWithM_ :: Applicative f => (Int -> a -> b -> f c) -> [a] -> [b] -> f ()
izipWithM_ f as bs = sequenceA_ (izipWith f as bs)
{-# INLINE izipWithM_ #-}
