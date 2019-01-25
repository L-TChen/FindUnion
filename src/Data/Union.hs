{-# LANGUAGE TupleSections #-}

module Data.Union
  ( Union (..)
  , buildDS
  , buildDSWith
  , toList
  , fromList
  , fromListWith
  , connected
  , find
  , size
  , component
  , components
  ) where 

import Data.Array.Base (unsafeAt)
import Data.Array

import Data.Union.ST (Union(..), buildDS, buildDSWith)
import Data.Union.Internal hiding (toList, fromList)

instance (Ix i, Show i, Show m) => Show (Union i m) where
  show u = showString "fromList " 
           $ shows (bounds $ getUnion u) $ showString " " $ show $ toList u

toList :: (Ix i) => Union i m -> [m]
toList = map trd3 . components 

fromList :: (Ix i) => (i, i) -> [[i]] -> Union i [i]
fromList bnd = fromListWith bnd (:[])

fromListWith :: (Ix i, Semigroup m) => (i, i) -> (i -> m) -> [[i]] -> Union i m
fromListWith bnd f = buildDSWith bnd f . concatMap pairs
  where 
    pairs []     = [] -- shall not include this clause 
    pairs (x:xs) = map (x,) xs 

connected :: (Ix i) => i -> i -> Union i m -> Bool 
connected i j u = find i u == find j u

find :: (Ix i, Eq i) => i -> Union i m -> i
find i (Union u) = fst3 $ u!i 

size :: (Ix i) => i -> Union i m -> Int
size i u'@(Union u) = snd3 (u ! find i u')

component :: (Ix i) => i -> Union i m -> m
component i u'@(Union u) = trd3 (u ! find i u')

components :: (Ix i) => Union i m -> [(i, Int, m)]
components (Union u) = filter (\(_, s, _) -> s /= 0) $ elems u
