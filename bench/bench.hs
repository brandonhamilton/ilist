import Data.List
import Data.List.Index

import Criterion
import Criterion.Main


main :: IO ()
main = defaultMain [
  bgroup "ifoldl" [
      bench "zip" $ nf (\n -> ifoldl_zip (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldl (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000 ],

  bgroup "ifoldl'" [
      bench "zip" $ nf (\n -> ifoldl'_zip (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000,
      bench "our" $ nf (\n -> ifoldl' (\i a x -> if rem x 16 == 0 then a+3*i else a+x) 0 [0..n]) 100000 ] ]

{-# INLINE ifoldl_zip #-}
ifoldl_zip :: (Int -> b -> el -> b) -> b -> [el] -> b
ifoldl_zip f a xs = foldl (\acc (i, x) -> f i acc x) a (zip [0..] xs)

{-# INLINE ifoldl'_zip #-}
ifoldl'_zip :: (Int -> b -> el -> b) -> b -> [el] -> b
ifoldl'_zip f a xs = foldl' (\acc (i, x) -> f i acc x) a (zip [0..] xs)
