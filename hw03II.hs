------------------------------------------------------------------------
-- Written by: Pedro de Oliveira Lira - pdeolive@syr.edu
------------------------------------------------------------------------
-- Homework 3, Part II

import Control.Monad( liftM, liftM2 )
import Data.List( nub, sort   )
import Test.QuickCheck( quickCheck, sample,
                        Arbitrary( arbitrary ), Gen,
                        oneof, elements, sized  )


------------------------------------------------------------------------
-- Problem 1 -----------------------------------------------------------

same :: [Int] -> Bool
same [x,y] = if x == y then True else False
same xs = and (zipWith (==) xs (tail xs))


------------------------------------------------------------------------
-- Problem 2 -----------------------------------------------------------

squash :: (a->a->b) -> [a] -> [b]

squash f [] = []
squash f (fst:snd:[]) = [f fst snd]
squash f (fst:snd:rest) = [f fst snd] ++ squash f rest

squash' :: (a->a->b) -> [a] -> [b]
squash' f xs = zipWith (f) xs (tail xs)

------------------------------------------------------------------------
-- Problem 3 -----------------------------------------------------------

data BTree = Empty | Branch Int BTree BTree
     deriving (Show)
     
foldT :: a -> (Int -> a -> a -> a) -> BTree -> a
foldT emp brch Empty = emp
foldT emp brch (Branch n tl tr) = brch n vl vr
          where vl = foldT emp brch tl
                vr = foldT emp brch tr

bcount t = foldT 0 tally t
           where tally _ numl numr = 1 + numl + numr

bsum t = foldT 0 f t
           where f n vl vr = n + vl + vr

bmaxDepth t = foldT (-1) g t
           where g n vl vr = 1 + max (vl) (vr)


------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------

bmaxDepth_test1 = bmaxDepth t13 == 2
bmaxDepth_test2 = bmaxDepth t0 == 0
bmaxDepth_test3 = bmaxDepth t3 == -1


-- Function to call all my tests
-- It returns true if the test is sucessfull.

tests :: [(String, Bool)]
tests = [ ("bmaxDepth_test1",     bmaxDepth_test1)
        , ("bmaxDepth_test2",       bmaxDepth_test2)  
        , ("bmaxDepth_test3",  bmaxDepth_test3)]


squash_pred xs = (squash (+) ys == squash' (+) ys)
           where ys = map (`mod` 1000) xs

t10 = Branch 5 
     (Branch 5 
      (Branch 5 
       (Branch 5 Empty Empty) 
       (Branch 5 Empty (Branch 5 Empty Empty)))
      Empty)
     (Branch 5 
      (Branch 5 (Branch 5 Empty Empty) Empty)
      (Branch 5 Empty Empty))

t13 = Branch 1
     (Branch 1 Empty Empty)
     (Branch 1 Empty
             (Branch 1 Empty Empty))
             
t0 = Branch 13 Empty Empty

t3 = Empty