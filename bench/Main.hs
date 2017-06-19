{-# LANGUAGE
BangPatterns,
MagicHash
  #-}


import Control.Monad.Trans.State.Lazy

import Criterion
import Criterion.Main

import qualified Control.Lens as L
import Data.List
import Data.List.Index
import Functions


main :: IO ()
main = defaultMain [

  bgroup "indexed" [
      bgroup "consume" [
          bench "zip" $ nf (\n -> length (indexed_zip [0..n])) (100000::Int),
          bench "vec" $ nf (\n -> length (indexed_vec [0..n])) (100000::Int),
          bench "rec" $ nf (\n -> length (indexed_rec [0..n])) (100000::Int),
          bench "fold" $ nf (\n -> length (indexed_fold [0..n])) (100000::Int),
          bench "lens" $ nf (\n -> length (L.itoList [0..n])) (100000::Int),
          bench "our" $ nf (\n -> length (indexed [0..n])) (100000::Int) ],
      bgroup "full" [
          bench "zip" $ nf (\n -> indexed_zip [0..n]) (100000::Int),
          bench "vec" $ nf (\n -> indexed_vec [0..n]) (100000::Int),
          bench "rec" $ nf (\n -> indexed_rec [0..n]) (100000::Int),
          bench "fold" $ nf (\n -> indexed_fold [0..n]) (100000::Int),
          bench "lens" $ nf (\n -> L.itoList [0..n]) (100000::Int),
          bench "our" $ nf (\n -> indexed [0..n]) (100000::Int) ] ],

  bgroup "deleteAt" [
      bgroup "consume" [
          bench "fold" $ nf (\n -> length (deleteAt_fold 1000 [0..n])) (100000::Int),
          bench "rec" $ nf (\n -> length (deleteAt_rec 1000 [0..n])) (100000::Int),
          bench "our" $ nf (\n -> length (deleteAt 1000 [0..n])) (100000::Int) ],
      bgroup "full" [
          bench "fold" $ nf (\n -> deleteAt_fold 1000 [0..n]) (100000::Int),
          bench "rec" $ nf (\n -> deleteAt_rec 1000 [0..n]) (100000::Int),
          bench "our" $ nf (\n -> deleteAt 1000 [0..n]) (100000::Int) ] ],

  bgroup "iall" [
      bgroup "full" [
          bench "zip" $ nf (\n -> iall_zip (==) [0..n]) 100000,
          bench "map" $ nf (\n -> iall_map (==) [0..n]) 100000,
          bench "rec" $ nf (\n -> iall_rec (==) [0..n]) 100000,
          bench "lens" $ nf (\n -> L.iall (==) [0..n]) 100000,
          bench "our" $ nf (\n -> iall (==) [0..n]) 100000 ],
      bgroup "early" [
          bench "zip" $ nf (\n -> iall_zip (/=) [0..n]) 100000,
          bench "map" $ nf (\n -> iall_map (/=) [0..n]) 100000,
          bench "rec" $ nf (\n -> iall_rec (/=) [0..n]) 100000,
          bench "lens" $ nf (\n -> L.iall (/=) [0..n]) 100000,
          bench "our" $ nf (\n -> iall (/=) [0..n]) 100000 ] ],

  bgroup "imap" [
      bgroup "consume" [
          bench "rec" $ nf (\n -> sum $ imap_rec (+) [0..n]) 100000,
          bench "fold" $ nf (\n -> sum $ imap_fold (+) [0..n]) 100000,
          bench "zip" $ nf (\n -> sum $ imap_zip (+) [0..n]) 100000,
          bench "vec" $ nf (\n -> sum $ imap_vec (+) [0..n]) 100000,
          bench "lens" $ nf (\n -> sum $ L.imap (+) [0..n]) 100000,
          bench "our" $ nf (\n -> sum $ imap (+) [0..n]) 100000 ],
      bgroup "full" [
          bench "rec" $ nf (\n -> imap_rec (+) [0..n]) 100000,
          bench "fold" $ nf (\n -> imap_fold (+) [0..n]) 100000,
          bench "zip" $ nf (\n -> imap_zip (+) [0..n]) 100000,
          bench "vec" $ nf (\n -> imap_vec (+) [0..n]) 100000,
          bench "lens" $ nf (\n -> L.imap (+) [0..n]) 100000,
          bench "our" $ nf (\n -> imap (+) [0..n]) 100000 ] ],

  bgroup "imapM" [
      bgroup "Just" [
          bench "zip" $ nf (\n -> imapM_zip (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> imapM_zipWith (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "rec" $ nf (\n -> imapM_rec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "vec" $ nf (\n -> imapM_vec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "lens" $ nf (\n -> L.imapM (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "our" $ nf (\n -> imapM (\i x -> if i==x then Just i else Nothing) [0..n]) 100000 ],
      bgroup "State" [
          bench "zip" $ nf (\n -> flip runState [] $ imapM_zip (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> flip runState [] $ imapM_zipWith (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "rec" $ nf (\n -> flip runState [] $ imapM_rec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "vec" $ nf (\n -> flip runState [] $ imapM_vec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "lens" $ nf (\n -> flip runState [] $ L.imapM (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "our" $ nf (\n -> flip runState [] $ imapM (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000 ] ],

  bgroup "imapM_" [
      bgroup "Just" [
          bench "zip" $ nf (\n -> imapM__zip (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> imapM__zipWith (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "rec" $ nf (\n -> imapM__rec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "vec" $ nf (\n -> imapM__vec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "lens" $ nf (\n -> L.imapM_ (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "our" $ nf (\n -> imapM_ (\i x -> if i==x then Just i else Nothing) [0..n]) 100000 ],
      bgroup "State" [
          bench "zip" $ nf (\n -> flip runState [] $ imapM__zip (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> flip runState [] $ imapM__zipWith (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "rec" $ nf (\n -> flip runState [] $ imapM__rec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "vec" $ nf (\n -> flip runState [] $ imapM__vec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "lens" $ nf (\n -> flip runState [] $ L.imapM_ (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "our" $ nf (\n -> flip runState [] $ imapM_ (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000 ] ],

  bgroup "ireplicateM_" [
      bgroup "Just" [
          bench "loop" $ nf (\n -> ireplicateM__loop n (\i -> if i<50000 then Just i else Nothing)) 100000,
          bench "for" $ nf (\n -> ireplicateM__for n (\i -> if i<50000 then Just i else Nothing)) 100000,
          bench "our" $ nf (\n -> ireplicateM_ n (\i -> if i<50000 then Just i else Nothing)) 100000 ],
      bgroup "State" [
          bench "loop" $ nf (\n -> flip runState 0 $ ireplicateM__loop n (\i -> modify' (i+))) 100000,
          bench "for" $ nf (\n -> flip runState 0 $ ireplicateM__for n (\i -> modify' (i+))) 100000,
          bench "our" $ nf (\n -> flip runState 0 $ ireplicateM_ n (\i -> modify' (i+))) 100000 ] ],

  bgroup "ifilter" [
      bgroup "consume" [
          bench "rec" $ nf (\n -> sum $ ifilter_rec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "fold" $ nf (\n -> sum $ ifilter_fold (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "zip" $ nf (\n -> sum $ ifilter_zip (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "vec" $ nf (\n -> sum $ ifilter_vec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "our" $ nf (\n -> sum $ ifilter (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "non-indexed" $ nf (\n -> sum $ filter (\x -> rem x 5000 == 0) [0..n]) (100000::Int) ],
      bgroup "full" [
          bench "rec" $ nf (\n -> ifilter_rec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "fold" $ nf (\n -> ifilter_fold (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "zip" $ nf (\n -> ifilter_zip (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "vec" $ nf (\n -> ifilter_vec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "our" $ nf (\n -> ifilter (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "non-indexed" $ nf (\n -> filter (\x -> rem x 5000 == 0) [0..n]) (100000::Int) ] ],

  bgroup "ifindIndices" [
      bgroup "consume" [
          bench "rec" $ nf (\n -> sum $ ifindIndices_rec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "fold" $ nf (\n -> sum $ ifindIndices_fold (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "zip" $ nf (\n -> sum $ ifindIndices_zip (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "our" $ nf (\n -> sum $ ifindIndices (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000 ],
      bgroup "full" [
          bench "rec" $ nf (\n -> ifindIndices_rec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "fold" $ nf (\n -> ifindIndices_fold (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "zip" $ nf (\n -> ifindIndices_zip (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
          bench "our" $ nf (\n -> ifindIndices (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000 ] ],

  bgroup "ifoldr" [
      bench "zip" $ nf (\n -> ifoldr_zip (\i x a -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "vec" $ nf (\n -> ifoldr_vec (\i x a -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "lens" $ nf (\n -> L.ifoldr (\i x a -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldr (\i x a -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "non-indexed" $ nf (\n -> foldr (\x a -> if rem x 16 == 0 then a+3*x else a+x) 0 [0..n]) (100000::Int) ],

{-

  bgroup "ifoldr1" [
      bench "zip" $ nf (\n -> ifoldr1_zip (\i x a -> if rem x 16 == 0 then a+3*i else a+x) [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldr1 (\i x a -> if rem x 16 == 0 then a+3*i else a+x) [0..n]) 100000 ],

-}

  bgroup "ifoldl" [
      bench "zip" $ nf (\n -> ifoldl_zip (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "fold" $ nf (\n -> ifoldl_fold (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "vec" $ nf (\n -> ifoldl_vec (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "lens" $ nf (\n -> L.ifoldl (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldl (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "non-indexed" $ nf (\n -> foldl (\a x -> if rem x 16 == 0 then a+3*x else a+x) 0 [0..n]) (100000::Int) ],

  bgroup "ifoldl'" [
      bgroup "if" [
          bench "zip" $ nf (\n -> ifoldl'_zip (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
          bench "fold" $ nf (\n -> ifoldl'_fold (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
          bench "vec" $ nf (\n -> ifoldl'_vec (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
          bench "lens" $ nf (\n -> L.ifoldl' (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
          bench "our" $ nf (\n -> ifoldl' (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
          bench "non-indexed" $ nf (\n -> foldl' (\a x -> if rem x 16 == 0 then a+3*x else a+x) 0 [0..n]) (100000::Int) ],
      bgroup "plus" [
          bench "zip" $ nf (\n -> ifoldl'_zip (\a i x -> a+i+x) 0 [0..n]) 100000,
          bench "fold" $ nf (\n -> ifoldl'_fold (\a i x -> a+i+x) 0 [0..n]) 100000,
          bench "vec" $ nf (\n -> ifoldl'_vec (\a i x -> a+i+x) 0 [0..n]) 100000,
          bench "lens" $ nf (\n -> L.ifoldl' (\i a x -> a+i+x) 0 [0..n]) 100000,
          bench "our" $ nf (\n -> ifoldl' (\a i x -> a+i+x) 0 [0..n]) 100000,
          bench "non-indexed" $ nf (\n -> foldl' (\a x -> a+x) 0 [0..n]) (100000::Int) ] ],

  bgroup "imapAccumR" [
      bench "rec" $ nf (\n -> imapAccumR_rec (\a i x -> (2*a+i*x, x*2)) 0 [0..n]) 100000,
      bench "lens" $ nf (\n -> L.imapAccumR (\i a x -> (2*a+i*x, x*2)) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> imapAccumR (\a i x -> (2*a+i*x, x*2)) 0 [0..n]) 100000,
      bench "non-indexed" $ nf (\n -> mapAccumR (\a x -> (2*a+a*x, x*2)) (0::Int) [0..n]) 100000 ],

  bgroup "imapAccumL" [
      bench "rec" $ nf (\n -> imapAccumL_rec (\a i x -> (2*a+i*x, x*2)) 0 [0..n]) 100000,
      bench "lens" $ nf (\n -> imapAccumL (\i a x -> (2*a+i*x, x*2)) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> imapAccumL (\a i x -> (2*a+i*x, x*2)) 0 [0..n]) 100000,
      bench "non-indexed" $ nf (\n -> mapAccumL (\a x -> (2*a+a*x, x*2)) (0::Int) [0..n]) 100000 ],

  bgroup "ifoldrM" [
      bgroup "Just" [
          bench "lens" $ nf (\n -> L.ifoldrM (\i x a -> if i==x then Just (i+a+x) else Nothing) 0 [0..n]) 100000,
          bench "our" $ nf (\n -> ifoldrM (\i x a -> if i==x then Just (i+a+x) else Nothing) 0 [0..n]) 100000 ] ],

  bgroup "ifoldlM" [
      bgroup "Just" [
          bench "lens" $ nf (\n -> L.ifoldlM (\i a x -> if i==x then Just (i+a+x) else Nothing) 0 [0..n]) 100000,
          bench "our" $ nf (\n -> ifoldlM (\a i x -> if i==x then Just (i+a+x) else Nothing) 0 [0..n]) 100000 ] ],
  
  bgroup "izipWith" [
      bgroup "consume" [
          bench "rec" $ nf (\n -> sum $ izipWith_rec (\i x y -> i+x+y) [0..n] [0..n]) 100000,
          bench "vec" $ nf (\n -> sum $ izipWith_vec (\i x y -> i+x+y) [0..n] [0..n]) 100000,
          bench "our" $ nf (\n -> sum $ izipWith (\i x y -> i+x+y) [0..n] [0..n]) 100000,
          bench "non-indexed" $ nf (\n -> sum $ zipWith (\x y -> x+y) [0..n] [0..n]) (100000::Int) ],
      bgroup "full" [
          bench "rec" $ nf (\n -> izipWith_rec (\i x y -> i+x+y) [0..n] [0..n]) 100000,
          bench "vec" $ nf (\n -> izipWith_vec (\i x y -> i+x+y) [0..n] [0..n]) 100000,
          bench "our" $ nf (\n -> izipWith (\i x y -> i+x+y) [0..n] [0..n]) 100000,
          bench "non-indexed" $ nf (\n -> zipWith (\x y -> x+y) [0..n] [0..n]) (100000::Int) ] ],

  bgroup "izipWithM" [
      bgroup "Just" [
          bench "rec" $ nf (\n -> izipWithM_rec (\i x y -> if i==x&&x==y then Just i else Nothing) [0..n] [0..n]) 100000,
          bench "vec" $ nf (\n -> izipWithM_vec (\i x y -> if i==x&&x==y then Just i else Nothing) [0..n] [0..n]) 100000,
          bench "our" $ nf (\n -> izipWithM (\i x y -> if i==x&&x==y then Just i else Nothing) [0..n] [0..n]) 100000 ],
      bgroup "State" [
          bench "rec" $ nf (\n -> flip runState [] $ izipWithM_rec (\i x y -> modify ((i+x+y):) >> return (i-x)) [0..n] [0..n]) 100000,
          bench "vec" $ nf (\n -> flip runState [] $ izipWithM_vec (\i x y -> modify ((i+x+y):) >> return (i-x)) [0..n] [0..n]) 100000,
          bench "our" $ nf (\n -> flip runState [] $ izipWithM (\i x y -> modify ((i+x+y):) >> return (i-x)) [0..n] [0..n]) 100000 ] ],

  bgroup "izipWithM_" [
      bgroup "Just" [
          bench "rec" $ nf (\n -> izipWithM__rec (\i x y -> if i==x&&x==y then Just i else Nothing) [0..n] [0..n]) 100000,
          bench "vec" $ nf (\n -> izipWithM__vec (\i x y -> if i==x&&x==y then Just i else Nothing) [0..n] [0..n]) 100000,
          bench "our" $ nf (\n -> izipWithM_ (\i x y -> if i==x&&x==y then Just i else Nothing) [0..n] [0..n]) 100000 ],
      bgroup "State" [
          bench "rec" $ nf (\n -> flip runState [] $ izipWithM__rec (\i x y -> modify ((i+x+y):) >> return (i-x)) [0..n] [0..n]) 100000,
          bench "vec" $ nf (\n -> flip runState [] $ izipWithM__vec (\i x y -> modify ((i+x+y):) >> return (i-x)) [0..n] [0..n]) 100000,
          bench "our" $ nf (\n -> flip runState [] $ izipWithM_ (\i x y -> modify ((i+x+y):) >> return (i-x)) [0..n] [0..n]) 100000 ] ] ]
