{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}
module JSFunc where
  
import Control.Category.Cartesian
import Control.Category.Monoidal  
import Data.Text ( Text )
import Data.String.Interpolate ( i )
import Control.Category (Category (..))
import Data.Profunctor.Unsafe
import Data.Profunctor.Choice
import MyPrimitives
import Numerics
  
newtype JSFunc a b = 
    JSFunc {renderJS :: Text} deriving Show

instance Category JSFunc where
  id = JSFunc "x => "
  (.) :: JSFunc y z -> JSFunc x y -> JSFunc x z
  f . g = JSFunc [i|(input) => {
  const fst = #{renderJS f};
  const snd = #{renderJS g};
  return fst(snd(input));
  }|]

instance Cartesian JSFunc where
  copy    = JSFunc "(x => ([x, x]))"
  consume = JSFunc "x => null"
  fst'    = JSFunc "(([x, _]) => x)"
  snd'    = JSFunc "(([_, y]) => y)"
  
instance MonoidalProduct JSFunc where
  first' f = JSFunc [i|([l, r]) => {
  const onFirst = #{renderJS f};
  const result = onFirst(l); 
  return [result, r];
  }|]
  
instance SymmetricProduct JSFunc where
  swap :: JSFunc (l, r) (r, l)
  swap = JSFunc [i|(([l, r]) => [r, l])|]

  reassoc = undefined  

instance MyPrimitives JSFunc where
  reverseString = JSFunc [i|((s) => s.split("").reverse().join(''))|]
  eq            = JSFunc [i|(([x, y]) => x === y)|]


instance SymmetricSum JSFunc where
  swapE = undefined
  reassocE = undefined

instance MonoidalSum JSFunc where
  left :: JSFunc a b -> JSFunc (Either a c) (Either b c)
  left (JSFunc t) = JSFunc t 
  right :: JSFunc a b -> JSFunc (Either c a) (Either c b)
  right (JSFunc f) = JSFunc f 

  
instance Cocartesian JSFunc where  
  injectL = JSFunc [i|(x => ({tag: 'left', value: x}))|]
  injectR = JSFunc [i|(x => ({tag: 'right', value: x}))|]
  unify   = JSFunc [i|(x => (x.value))|]
  tag     = JSFunc [i|(([b, x]) => ({tag: b ? 'right' : 'left', value: x}))|]

instance Profunctor JSFunc where
  lmap = undefined
  rmap = undefined

instance Choice JSFunc where
  left' f = JSFunc [i|(input) => {
  const overLeft = #{renderJS f};
  if (input.tyg == 'left') {
    return {tag: 'left', value: overLeft(input.value)};
  }
  return input
  }|]

instance Numeric JSFunc where
  num n   = JSFunc [i|(x) => (#{n})|]
  negate' = JSFunc [i|(x) => (-x)|]
  add     = JSFunc [i|([x, y]) => (x + y)|]
  mult    = JSFunc [i|([x, y]) => (x * y)|]
  div'    = JSFunc [i|([x, y]) => (x / y)|]
  mod'    = JSFunc [i|([x, y]) => (x % y)|]
  