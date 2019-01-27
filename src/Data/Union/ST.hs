{-# LANGUAGE BangPatterns, Trustworthy #-} 

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Union.ST
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Liang-Ting Chen
-- Stability   :  experimental
-- Portability :  non-portable

-----------------------------------------------------------------------------

module Data.Union.ST
  ( STUnion
  , Union (..)
  -- * Creation
  , create
  , createWith
  , buildDS
  , buildDSWith
  -- * Find-related operations 
  , find
  , connected
  , component
  , components
  -- * Union-related operations
  , union
  , unions
  ) where

import Data.Array
import Data.Array.ST 
import Data.Array.Base (unsafeRead, unsafeWrite)

import Control.Monad
import Control.Monad.ST

import Data.Union.Internal

-- | The type of collections of disjoint unions to be used within the 'Control.Monad.ST'.
type STUnion s i m = STArray s i (i, Int, m) 

-- | The type of collections of disjoint unions using persistent array.
newtype Union i m = Union { getUnion :: Array i (i, Int, m) -- ^ Retrieve the underlying array representation
                    }
parent :: (Ix i) => i -> STUnion s i m -> ST s i
parent i u = fst3 <$> readArray u i

-- | Check if two representatives are in the same set.
connected :: (Ix i, Semigroup m) => i -> i -> STUnion s i m -> ST s Bool
connected i j u = (==) <$> find i u <*> find j u

size :: (Ix i) => (i, Int, m) -> Int
size (_, n, _) = n

-- | Return the set of elements containing the given argument (after applying the monoid operation)
component :: (Ix i, Semigroup m) => i -> STUnion s i m -> ST s m
component i u = find i u >>= fmap trd3 . readArray u

-- | Return the collection of disjoint sets of type 'm', its size, and its representative. 
components :: (Ix i, Semigroup m) => STUnion s i m -> ST s [(i, Int, m)] 
components u = do 
  bnd <- getBounds u
  rebuild bnd u 
  xss <- getAssocs u
  return [ (par, s, xs) | (i, (par, s, xs)) <- xss, s /= 0 ]

-- | Find the representative of the set to which the arguments belongs.
find :: (Ix i, Eq i, Semigroup m) => i -> STUnion s i m -> ST s i
find i u = do
  !j <- parent i u
  if i == j then return j else do
    !bnd <- getBounds u
    compress bnd i j u 

-- | Create an *n*-element collection of singleton disjoint sets.
create :: (Ix i) => (i, i) -> ST s (STUnion s i [i])
create bnd = createWith bnd (:[])

-- | Similar to the above but assigning to every representative an element of some semigroup.
createWith :: (Ix i, Semigroup m) => (i, i) -> (i -> m) -> ST s (STUnion s i m)
createWith bnd f = newListArray bnd $ zip3 xs [1,1..] (map f xs)
  where 
    xs = range bnd

-- | Union of two sets which contain the arguments respectively.
union :: (Ix i, Semigroup m) => i -> i -> STUnion s i m -> ST s ()
union i j u = do
  !pi <- find i u 
  !pj <- find j u 
  if pi == pj then return () else do 
    !bnd <- getBounds u
    x <- unsafeRead u (index bnd pi)
    y <- unsafeRead u (index bnd pj)
    if size x < size y then link bnd x y u else link bnd y x u

-- | Similar to the above, but takes a set of pairs instead. 
unions :: (Ix i, Semigroup m) => [(i, i)] -> STUnion s i m -> ST s ()
unions reps u = forM_ reps (\(i, j) -> union i j u)

compress :: (Ix i, Semigroup m) => (i, i) -> i -> i -> STUnion s i m -> ST s i
compress bnd i j u = do
    (_, _, xs) <- unsafeRead u (index bnd i) 
    !pi        <- unsafeFind bnd j u 
    unsafeWrite u (index bnd i) (pi, 0, xs) 
    return pi

link :: (Ix i, Semigroup m) 
    => (i, i) -> (i, Int, m) -> (i, Int, m) -> STUnion s i m -> ST s ()
link bnd (i, !n, xs) (j, !m, ys) u = do
  unsafeWrite u (index bnd i) (j, 0,   xs)
  unsafeWrite u (index bnd j) (j, n+m, xs <> ys)

rebuild :: (Ix i, Semigroup m) => (i, i) -> STUnion s i m -> ST s () 
rebuild bnd u = forM_ (range bnd) $ \i -> unsafeFind bnd i u

-- | Create a collection of disjoint sets using 'union'. 
buildDS :: (Ix i) => (i, i) -> [(i, i)] -> Union i [i]
buildDS bnd = buildDSWith bnd (:[]) 

-- | Similar to the above, but takes an additional function assigning to to every representative an element of some semigroup. 
buildDSWith :: (Ix i, Semigroup m) => (i, i) -> (i -> m) -> [(i, i)] -> Union i m
buildDSWith bnd f reps = Union $ runSTArray $ do 
  u <- createWith bnd f 
  unions reps u 
  rebuild bnd u 
  return u

unsafeParent :: (Ix i, Semigroup m) => (i, i) -> i -> STUnion s i m -> ST s i
unsafeParent bnd i u = fst3 <$> unsafeRead u (index bnd i)

unsafeFind :: (Ix i, Eq i, Semigroup m) => (i, i) -> i -> STUnion s i m -> ST s i
unsafeFind bnd i u = do
  !j <- unsafeParent bnd i u
  if i == j then return j else compress bnd i j u 
