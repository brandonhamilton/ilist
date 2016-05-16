{-# LANGUAGE
BangPatterns,
MagicHash
  #-}


import Control.Monad.Trans.State.Lazy

import Criterion
import Criterion.Main

import Data.List.Index
import Functions


main :: IO ()
main = defaultMain [

  bgroup "iall" [
      bgroup "full" [
          bench "zip" $ nf (\n -> iall_zip (==) [0..n]) 100000,
          bench "map" $ nf (\n -> iall_map (==) [0..n]) 100000,
          bench "rec" $ nf (\n -> iall_rec (==) [0..n]) 100000,
          bench "our" $ nf (\n -> iall (==) [0..n]) 100000 ],
      bgroup "early" [
          bench "zip" $ nf (\n -> iall_zip (/=) [0..n]) 100000,
          bench "map" $ nf (\n -> iall_map (/=) [0..n]) 100000,
          bench "rec" $ nf (\n -> iall_rec (/=) [0..n]) 100000,
          bench "our" $ nf (\n -> iall (/=) [0..n]) 100000 ] ],

  bgroup "imap" [
      bgroup "consume" [
          bench "rec" $ nf (\n -> sum $ imap_rec (+) [0..n]) 100000,
          bench "fold" $ nf (\n -> sum $ imap_fold (+) [0..n]) 100000,
          bench "zip" $ nf (\n -> sum $ imap_zip (+) [0..n]) 100000,
          bench "vec" $ nf (\n -> sum $ imap_vec (+) [0..n]) 100000,
          bench "our" $ nf (\n -> sum $ imap (+) [0..n]) 100000 ],
      bgroup "full" [
          bench "rec" $ nf (\n -> imap_rec (+) [0..n]) 100000,
          bench "fold" $ nf (\n -> imap_fold (+) [0..n]) 100000,
          bench "zip" $ nf (\n -> imap_zip (+) [0..n]) 100000,
          bench "vec" $ nf (\n -> imap_vec (+) [0..n]) 100000,
          bench "our" $ nf (\n -> imap (+) [0..n]) 100000 ] ],

  bgroup "imapM" [
      bgroup "Just" [
          bench "zip" $ nf (\n -> imapM_zip (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> imapM_zipWith (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "rec" $ nf (\n -> imapM_rec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "vec" $ nf (\n -> imapM_vec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "our" $ nf (\n -> imapM (\i x -> if i==x then Just i else Nothing) [0..n]) 100000 ],
      bgroup "State" [
          bench "zip" $ nf (\n -> flip runState [] $ imapM_zip (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> flip runState [] $ imapM_zipWith (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "rec" $ nf (\n -> flip runState [] $ imapM_rec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "vec" $ nf (\n -> flip runState [] $ imapM_vec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "our" $ nf (\n -> flip runState [] $ imapM (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000 ] ],

  bgroup "imapM_" [
      bgroup "Just" [
          bench "zip" $ nf (\n -> imapM__zip (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> imapM__zipWith (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "rec" $ nf (\n -> imapM__rec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "vec" $ nf (\n -> imapM__vec (\i x -> if i==x then Just i else Nothing) [0..n]) 100000,
          bench "our" $ nf (\n -> imapM_ (\i x -> if i==x then Just i else Nothing) [0..n]) 100000 ],
      bgroup "State" [
          bench "zip" $ nf (\n -> flip runState [] $ imapM__zip (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "zipWith" $ nf (\n -> flip runState [] $ imapM__zipWith (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "rec" $ nf (\n -> flip runState [] $ imapM__rec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "vec" $ nf (\n -> flip runState [] $ imapM__vec (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000,
          bench "our" $ nf (\n -> flip runState [] $ imapM_ (\i x -> modify ((i+x):) >> return (i-x)) [0..n]) 100000 ] ],

  bgroup "ifilter" [
      bench "rec" $ nf (\n -> ifilter_rec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
      bench "fold" $ nf (\n -> ifilter_fold (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
      bench "zip" $ nf (\n -> ifilter_zip (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
      bench "vec" $ nf (\n -> ifilter_vec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
      bench "our" $ nf (\n -> ifilter (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000 ],

  bgroup "ifindIndices" [
      bench "rec" $ nf (\n -> ifindIndices_rec (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
      bench "fold" $ nf (\n -> ifindIndices_fold (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
      bench "zip" $ nf (\n -> ifindIndices_zip (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000,
      bench "our" $ nf (\n -> ifindIndices (\i x -> rem (i+x) 5000 == 0) [0..n]) 100000 ],

  bgroup "ifoldr" [
      bench "zip" $ nf (\n -> ifoldr_zip (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "vec" $ nf (\n -> ifoldr_vec (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldr (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000 ],

{-

  bgroup "ifoldr1" [
      bench "zip" $ nf (\n -> ifoldr1_zip (\i a x -> if rem x 16 == 0 then a+3*i else a+x) [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldr1 (\i a x -> if rem x 16 == 0 then a+3*i else a+x) [0..n]) 100000 ],

-}

  bgroup "ifoldl" [
      bench "zip" $ nf (\n -> ifoldl_zip (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "fold" $ nf (\n -> ifoldl_fold (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "vec" $ nf (\n -> ifoldl_vec (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldl (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000 ],

  bgroup "ifoldl'" [
      bench "zip" $ nf (\n -> ifoldl'_zip (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "fold" $ nf (\n -> ifoldl'_fold (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "vec" $ nf (\n -> ifoldl'_vec (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldl' (\a i x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000 ],

  bgroup "imapAccumR" [
      bench "rec" $ nf (\n -> imapAccumR_rec (\a i x -> (2*a+i*x, x*2)) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> imapAccumR (\a i x -> (2*a+i*x, x*2)) 0 [0..n]) 100000 ],

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
