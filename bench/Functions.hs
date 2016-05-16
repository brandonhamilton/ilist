{-# LANGUAGE
MagicHash,
BangPatterns
  #-}


-- All these functions have to be in a separate module because otherwise
-- fusion breaks for some reason (I've spent a day trying to understand why
-- my definition of 'izipWith' (currently in Data.List.Index) wasn't fusing,
-- before I tried moving it into a different module and it started fusing).
module Functions where


import qualified Data.Vector as V
import GHC.Exts
import Data.List
import Data.List.Index
import Control.Monad


imapM_zip :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM_zip f xs = mapM (uncurry f) (zip [0..] xs)
{-# INLINE imapM_zip #-}

imapM_vec :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM_vec f xs = liftM V.toList (V.imapM f (V.fromList xs))
{-# INLINE imapM_vec #-}

imapM_zipWith :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM_zipWith f xs = zipWithM f [0..] xs
{-# INLINE imapM_zipWith #-}

imapM_rec :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM_rec f as = go 0# as
  where
    go _ [] = return []
    go i (x:xs) = do
      x' <- f (I# i) x
      xs' <- go (i +# 1#) xs
      return (x':xs')
{-# INLINE imapM_rec #-}

imapM__zip :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM__zip f xs = mapM_ (uncurry f) (zip [0..] xs)
{-# INLINE imapM__zip #-}

imapM__vec :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM__vec f xs = V.imapM_ f (V.fromList xs)
{-# INLINE imapM__vec #-}

imapM__zipWith :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM__zipWith f xs = zipWithM_ f [0..] xs
{-# INLINE imapM__zipWith #-}

imapM__rec :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM__rec f as = go 0# as
  where
    go _ [] = return ()
    go i (x:xs) = do
      f (I# i) x
      go (i +# 1#) xs
{-# INLINE imapM__rec #-}

iall_zip :: (Int -> a -> Bool) -> [a] -> Bool
iall_zip p xs = and (zipWith p [0..] xs)
{-# INLINE iall_zip #-}

iall_map :: (Int -> a -> Bool) -> [a] -> Bool
iall_map f xs = and (imap f xs)
{-# INLINE iall_map #-}

iall_rec :: (Int -> a -> Bool) -> [a] -> Bool
iall_rec p = go 0#
  where
    go _ [] = True
    go i (x:xs) = p (I# i) x && go (i +# 1#) xs
{-# INLINE iall_rec #-}

ifoldr_zip :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr_zip f a xs = foldr (\(i, x) acc -> f i x acc) a (zip [0..] xs)
{-# INLINE ifoldr_zip #-}

ifoldr_vec :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr_vec f a xs = V.ifoldr f a (V.fromList xs)
{-# INLINE ifoldr_vec #-}

{-
ifoldr1_zip :: (Int -> a -> a -> a) -> [a] -> a
ifoldr1_zip f xs = snd (foldr1 (\(i, x) (j, y) -> (j, f i x y)) (zip [0..] xs))
{-# INLINE ifoldr1_zip #-}
-}

ifoldl_zip :: (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl_zip f a xs = foldl (\acc (!i, x) -> f acc i x) a (zip [0..] xs)
{-# INLINE ifoldl_zip #-}

ifoldl_vec :: (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl_vec f a xs = V.ifoldl f a (V.fromList xs)
{-# INLINE ifoldl_vec #-}

ifoldl'_zip :: (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl'_zip f a xs = foldl' (\acc (!i, x) -> f acc i x) a (zip [0..] xs)
{-# INLINE ifoldl'_zip #-}

ifoldl'_vec :: (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl'_vec f a xs = V.ifoldl' f a (V.fromList xs)
{-# INLINE ifoldl'_vec #-}

ifoldl_fold :: (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl_fold f z xs = foldl (\g x !i -> f (g (i-1)) i x) (const z) xs (length xs - 1)
{-# INLINE ifoldl_fold #-}

ifoldl'_fold :: (b -> Int -> a -> b) -> b -> [a] -> b
ifoldl'_fold f z xs = foldl' (\g x !i -> f (g (i - 1)) i x) (const z) xs (length xs - 1)
{-# INLINE ifoldl'_fold #-}

imap_rec :: (Int -> a -> b) -> [a] -> [b]
imap_rec p = go 0#
  where
    go _ [] = []
    go i (x:xs) = p (I# i) x : go (i +# 1#) xs
{-# INLINE imap_rec #-}

imap_fold :: (Int -> a -> b) -> [a] -> [b]
imap_fold f = ifoldr (\i x xs -> f i x : xs) []
{-# INLINE imap_fold #-}

imap_zip :: (Int -> a -> b) -> [a] -> [b]
imap_zip p xs = zipWith p [0..] xs
{-# INLINE imap_zip #-}

imap_vec :: (Int -> a -> b) -> [a] -> [b]
imap_vec p xs = V.toList (V.imap p (V.fromList xs))
{-# INLINE imap_vec #-}

ifilter_rec :: (Int -> a -> Bool) -> [a] -> [a]
ifilter_rec p = go 0#
  where
    go _ [] = []
    go i (x:xs) | p (I# i) x = x : go (i +# 1#) xs
                | otherwise = go (i +# 1#) xs
{-# INLINE ifilter_rec #-}

ifilter_fold :: (Int -> a -> Bool) -> [a] -> [a]
ifilter_fold p = ifoldr (\i x xs -> if p i x then x : xs else xs) []
{-# INLINE ifilter_fold #-}

ifilter_zip :: (Int -> a -> Bool) -> [a] -> [a]
ifilter_zip p xs = map snd (filter (uncurry p) (zip [0..] xs))
{-# INLINE ifilter_zip #-}

ifilter_vec :: (Int -> a -> Bool) -> [a] -> [a]
ifilter_vec p xs = V.toList (V.ifilter p (V.fromList xs))
{-# INLINE ifilter_vec #-}

ifindIndices_rec :: (Int -> a -> Bool) -> [a] -> [Int]
ifindIndices_rec p = go 0#
  where
    go _ [] = []
    go i (x:xs) | p (I# i) x = I# i : go (i +# 1#) xs
                | otherwise  = go (i +# 1#) xs
{-# INLINE ifindIndices_rec #-}

ifindIndices_fold :: (Int -> a -> Bool) -> [a] -> [Int]
ifindIndices_fold p = ifoldr (\i x xs -> if p i x then i : xs else xs) []
{-# INLINE ifindIndices_fold #-}

ifindIndices_zip :: (Int -> a -> Bool) -> [a] -> [Int]
ifindIndices_zip p xs = map fst (filter (uncurry p) (zip [0..] xs))
{-# INLINE ifindIndices_zip #-}

izipWith_rec :: (Int -> a -> b -> c) -> [a] -> [b] -> [c]
izipWith_rec f = go 0#
  where
    go i (a:as) (b:bs) = f (I# i) a b : go (i +# 1#) as bs
    go _ _ _ = []
{-# INLINE izipWith_rec #-}

izipWith_vec :: (Int -> a -> b -> c) -> [a] -> [b] -> [c]
izipWith_vec f xs ys = V.toList (V.izipWith f (V.fromList xs) (V.fromList ys))
{-# INLINE izipWith_vec #-}
