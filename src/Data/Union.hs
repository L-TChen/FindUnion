{-# LANGUAGE TupleSections, Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Union.ST
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Liang-Ting Chen
-- Stability   :  experimental
-- Portability :  non-portable

-----------------------------------------------------------------------------

module Data.Union
  ( Union (..)
  -- * Creation of disjoint unions
  , buildDS
  , buildDSWith
  -- * Conversion between lists of lists and disjoint unions
  , fromList
  , fromListWith
  , toList
  -- * Find-related operations of Find-Union 
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

-- | Construct a collection of disjoint sets from a list of lists. Two lists are in the same set if there is any overlap. 
fromList :: (Ix i) => (i, i) -> [[i]] -> Union i [i]
fromList bnd = fromListWith bnd (:[])

-- | Similar to the above but takes an assignment of representatives to elements of a semigroup.
fromListWith :: (Ix i, Semigroup m) => (i, i) -> (i -> m) -> [[i]] -> Union i m
fromListWith bnd f = buildDSWith bnd f . concatMap pairs
  where 
    pairs []     = [] -- shall not include this clause 
    pairs (x:xs) = map (x,) xs 

-- | Return the list of disjoint lists
toList :: (Ix i) => Union i m -> [m]
toList = map trd3 . components 

-- | Check if a pair of representatives belong to the same set. 
connected :: (Ix i) => i -> i -> Union i m -> Bool 
connected i j u = find i u == find j u

-- | Return the canonical representative for the set containing the argument.
find :: (Ix i, Eq i) => i -> Union i m -> i
find i (Union u) = fst3 $ u!i 

-- | Return the size of set which contains the argument.
size :: (Ix i) => i -> Union i m -> Int
size i u'@(Union u) = snd3 (u ! find i u')

-- | Return the set containing the argument and apply semigroup operation to every element.
component :: (Ix i) => i -> Union i m -> m
component i u'@(Union u) = trd3 (u ! find i u')

-- | Return the collection of disjoint sets, their sizes and canonical representatives. 
components :: (Ix i) => Union i m -> [(i, Int, m)]
components (Union u) = filter (\(_, s, _) -> s /= 0) $ elems u
