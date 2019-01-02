import Integer
import List(nub)
import Maybe

import Test.Prop
import System.Random

import Data.Array

array = listToErrorArray 

allEntries n a = map (a!) [0 .. n]

testInitializeAndRetrieveFromArray =
  eq id (\xs -> allEntries (length xs - 1) (array xs))

upTest nums = 
  let maxidx = length nums - 1
      startArray = array (map Just nums)
      rndChanges = map (abs . flip mod maxidx) (take (div (maxidx+1) 3) nums)
      newArray   =  startArray // zip rndChanges (repeat Nothing)
   in compare rndChanges 0 nums (allEntries maxidx newArray)

testUpdateAlreadyInitializedPositions= test upTest

compare _ _ [] [] = True
compare nos n (_:xs) (Nothing:ys) = elem n nos && compare nos (n+1) xs ys
compare nos n (x:xs) (Just y:ys) = x==y && compare nos (n+1) xs ys



------------------------------------------------------------------------------
-- Random test:

--- Tests a given predicate on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
test :: ([Int] -> Bool) -> PropIO
test f =
  (rndList lenRnds >>= \xs -> return (if f xs then Nothing else Just xs))
  `returns` Nothing

--- Tests whether two operations return equal results
--- on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
eq :: Eq a => ([Int] -> a) -> ([Int] -> a) -> PropIO
eq f g = test (\x -> (f x)==(g x))

--- generate a list of at most n random numbers (without duplicated elements)
rndList :: Int -> IO [Int]
rndList n = getRandomSeed >>= return . nub . take n . (flip nextIntRange 100000)

--- maximal length of test lists
lenRnds :: Int
lenRnds = 1000

------------------------------------------------------------------------------
