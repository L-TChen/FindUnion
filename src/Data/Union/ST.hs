{-# LANGUAGE BangPatterns #-} 

module Data.Union.ST
  ( STUnion
  , Union (..)
  , create
  , createWith
  , connected
  , component
  , components
  , find
  , unions
  , buildDS
  , buildDSWith
  ) where

import Data.Array
import Data.Array.ST 
import Data.Array.Base (unsafeRead, unsafeWrite)

import Control.Monad
import Control.Monad.ST

import Data.Union.Internal

type STUnion s i m = STArray s i (i, Int, m) 
type Bound i = (i, i)

newtype Union i m = Union { getUnion :: Array i (i, Int, m) }

parent :: (Ix i) => i -> STUnion s i m -> ST s i
parent i u = fst3 <$> readArray u i

connected :: (Ix i, Semigroup m) => i -> i -> STUnion s i m -> ST s Bool
connected i j u = (==) <$> find i u <*> find j u

size :: (Ix i) => (i, Int, m) -> Int
size (_, n, _) = n

component :: (Ix i, Semigroup m) => i -> STUnion s i m -> ST s m
component i u = find i u >>= fmap trd3 . readArray u

components :: (Ix i, Semigroup m) => STUnion s i m -> ST s [(i, Int, m)] 
components u = do 
  bnd <- getBounds u
  rebuild bnd u 
  xss <- getAssocs u
  return [ (par, s, xs) | (i, (par, s, xs)) <- xss, s /= 0 ]

find :: (Ix i, Eq i, Semigroup m) => i -> STUnion s i m -> ST s i
find i u = do
  !j <- parent i u
  if i == j then return j else do
    !bnd <- getBounds u
    compress bnd i j u 

create :: (Ix i) => Bound i -> ST s (STUnion s i [i])
create bnd = createWith bnd (:[])

createWith :: (Ix i, Semigroup m) => Bound i -> (i -> m) -> ST s (STUnion s i m)
createWith bnd f = newListArray bnd $ zip3 xs [1,1..] (map f xs)
  where 
    xs = range bnd

union :: (Ix i, Semigroup m) => i -> i -> STUnion s i m -> ST s ()
union i j u = do
  !pi <- find i u 
  !pj <- find j u 
  if pi == pj then return () else do 
    !bnd <- getBounds u
    x <- unsafeRead u (index bnd pi)
    y <- unsafeRead u (index bnd pj)
    if size x < size y then link bnd x y u else link bnd y x u

unions :: (Ix i, Semigroup m) => [(i, i)] -> STUnion s i m -> ST s ()
unions reps u = forM_ reps (\(i, j) -> union i j u)

compress :: (Ix i, Semigroup m) => Bound i -> i -> i -> STUnion s i m -> ST s i
compress bnd i j u = do
    (_, _, xs) <- unsafeRead u (index bnd i) 
    !pi        <- unsafeFind bnd j u 
    unsafeWrite u (index bnd i) (pi, 0, xs) 
    return pi

link :: (Ix i, Semigroup m) 
    => Bound i -> (i, Int, m) -> (i, Int, m) -> STUnion s i m -> ST s ()
link bnd (i, !n, xs) (j, !m, ys) u = do
  unsafeWrite u (index bnd i) (j, 0,   xs)
  unsafeWrite u (index bnd j) (j, n+m, xs <> ys)

rebuild :: (Ix i, Semigroup m) => Bound i -> STUnion s i m -> ST s () 
rebuild bnd u = forM_ (range bnd) $ \i -> unsafeFind bnd i u

buildDS :: (Ix i) => Bound i -> [(i, i)] -> Union i [i]
buildDS bnd = buildDSWith bnd (:[]) 

buildDSWith :: (Ix i, Semigroup m) => Bound i -> (i -> m) -> [(i, i)] -> Union i m
buildDSWith bnd f reps = Union $ runSTArray $ do 
  u <- createWith bnd f 
  unions reps u 
  rebuild bnd u 
  return u

unsafeParent :: (Ix i, Semigroup m) => Bound i -> i -> STUnion s i m -> ST s i
unsafeParent bnd i u = fst3 <$> unsafeRead u (index bnd i)

unsafeFind :: (Ix i, Eq i, Semigroup m) => Bound i -> i -> STUnion s i m -> ST s i
unsafeFind bnd i u = do
  !j <- unsafeParent bnd i u
  if i == j then return j else compress bnd i j u 
