{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Category ( (>>>), Category(..) )
import Control.Category.Cartesian
    ( Cartesian(copy), Cocartesian(unify, tag) )
import Control.Category.Monoidal
    ( MonoidalProduct(first', second'), MonoidalSum((+++)) )

import JSFunc ( JSFunc(..) )
import MyPrimitives ( MyPrimitives(..) )
import Numerics ( Numeric(num, mod', mult, add, div') )
import Data.Text as T ( unpack )

{-
  In the following I'm implementing the code from Chris Penners Presentation
  at the Berlin FP Group https://www.youtube.com/watch?v=xZmPuz9m2t0
-}

thrice :: Category k => k a a -> k a a
thrice k = k >>> k >>> k

add3 :: Integer -> Integer
add3 = thrice (+ 1)

times10 :: JSFunc Int Int
times10 = JSFunc "x => x * 10"

times1000 :: JSFunc Int Int
times1000 = thrice times10

isPalindrome :: (Cartesian k, MyPrimitives k) => k String Bool
isPalindrome = 
      copy
  >>> first' reverseString
  >>> eq


-- branching
--collatzStep :: Int -> Int
--collatzStep n =
--  if even n 
--    then n `div` 2
--    else (3 * n) + 1

-- helper function

strong :: Cartesian k => k (a, b) c -> k a b -> k a c
strong f x = copy >>> second' x >>> f   

isEven :: forall k. (Numeric k, Cartesian k, MyPrimitives k) => k Int Bool
isEven = mod2 >>> strong eq (num 0)
  where
    mod2 :: k Int Int
    mod2 = strong mod' (num 2)
 
matchOn :: (Cartesian k, Cocartesian k) => k a Bool -> k a (Either a a)
matchOn predicate = copy >>> first' predicate >>> tag


collatzStep :: forall k. (Numeric k, Cartesian k, Cocartesian k, MyPrimitives k) => k Int Int
collatzStep = 
    matchOn isEven
    >>> (onOdds +++ onEvens)
    >>> unify
  where
    onOdds :: k Int Int
    onOdds = strong mult (num 3) >>> strong add (num 1)
    onEvens :: k Int Int
    onEvens = strong div' (num 2)

collatz' :: Int -> Int -> (Int, Int)
collatz' n count =
  if n == 1 
    then (n, count)
    else let step = collatzStep n
         in  collatz' step (count+1)

collatz :: forall k. (Numeric k, Cartesian k, Cocartesian k, MyPrimitives k) => k Int Int
collatz =
    matchOn (strong eq (num 1))
    >>> (onOther +++ onOne)
    >>> unify
  where
    onOther :: k Int Int
    onOther = 
      collatzStep >>> collatz
    onOne :: k Int Int
    onOne = num 1

main :: IO ()
main = do
  print $ thrice add3 10
  print $ renderJS times1000
  print $ isPalindrome "stressed desserts"
  print $ renderJS isPalindrome
  print $ collatz' 1000 0
  putStrLn $ T.unpack $ renderJS collatzStep


