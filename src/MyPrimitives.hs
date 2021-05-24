module MyPrimitives where
  
-- primitives
class MyPrimitives k where
  reverseString :: k String String
  eq            :: Eq a => k (a,a) Bool
 

instance MyPrimitives (->) where
  reverseString = Prelude.reverse
  eq = uncurry (==)
  

