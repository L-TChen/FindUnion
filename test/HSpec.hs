{-# LANGUAGE TupleSections, TypeApplications, ScopedTypeVariables, InstanceSigs, Rank2Types #-}


import Data.List (sort)
import qualified Data.Graph as G
import qualified Data.Tree  as T

import Data.Union

import Test.Hspec
import Test.QuickCheck
import System.Random

main :: IO ()
main = hspec $ do
  describe "buildDS" $ do
    it "postcomposed with toList is equal to Data.Graph.components up to permutation;" $
      property (withMaxSuccess 1000 prop_components)

  describe "toList . fromList" $ do
    it "is equal to identity up to permutation;" $
      property (withMaxSuccess 1000 prop_identity)

prop_components :: BoundedPairs -> Bool
prop_components (BoundedPairs bnd xss) = 
  (==) <$> sort . map sort . toList . buildDS bnd 
       <*> sort . map (sort . T.flatten) . G.components . G.buildG bnd
       $ xss

prop_identity :: BoundedPairs -> Bool
prop_identity (BoundedPairs bnd xss) = 
  let ds = buildDS bnd xss 
      xs = toList ds  
  in (==) <$> sort . map sort . toList . fromList bnd
          <*> sort . map sort
          $ xs 

data BoundedPairs = BoundedPairs (Int, Int) [(Int, Int)] deriving Show

instance Arbitrary BoundedPairs where 
  arbitrary = sized $ \n -> do
    i <- choose (1, n+1)
    let bnd = (1, i)
    BoundedPairs bnd <$> listOf1 (boundedPair bnd)
  
boundedPair :: (Random a) => (a, a) -> Gen (a, a)
boundedPair bnd = (,) <$> choose bnd <*> choose bnd
