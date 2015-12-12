module Main where

import Control.Monad.Random
import Data.Monoid
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import System.Random
import Control.Monad

main :: IO ()
main = undefined

data Tree a
    = Leaf a
    | Branch (Tree a) (Tree a)
    deriving (Show, Read, Eq)


-- -----------------------------------------------------------------------------
-- | Random Data Generation For Arbitrary Types
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- | Random Data Generation Using MonadRandom
--
--   * disadvantage is that values that values of types that are instances of
--      class Random have to be linearily ordered ...
--      This is not feasible for many important datastructures e.g. trees.
--      Solution: Use QuickCheck's class 'Arbitrary'.

randTree :: (Monad m, MonadRandom m, Random a) => Int -> m (Tree a)
randTree depth
    | depth <= 1 = Leaf <$> getRandom
    | otherwise = do
        depth1 <- randDepth
        depth2 <- randDepth
        Branch <$> randTree depth1 <*> randTree depth2
  where
    randDepth = getRandomR (1, pred depth)

-- Now random tree awesomness in ghci:
-- genTree 'a' 'c' 5
-- > <a random tree>

-- -----------------------------------------------------------------------------
-- | Random Data Generation Using QuickCkeck's Arbitrary
--
-- QuickCheck - Class Arbitrary
--   * The 'Random' typeclass requires a linear ordering of the values of its types.
--     QuickCheck's 'Arbitrary' drops that constraint.
--   * To proper instantiate the tree generator we have to change the generator
--     function to Constrain the type variable by 'Arbitrary' instead of 'Random'.
--   * As 'Gen' is also a monad we can use the same monadic style as above,
--     but with functions from the 'Test.QuickCheck.Gen' module.

genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree depth
    | depth <= 1 =  Leaf <$> arbitrary
    | otherwise = do
        depth1 <- genDepth
        depth2 <- genDepth
        Branch <$> genTree depth1 <*> genTree depth2
  where
    genDepth = elements [1 .. pred depth]

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized genTree

-- now you can check it out in ghci:
-- generate arbitrary :: IO Exp
-- > <a random tree>


-- | Example: Lambda Terms
--   * Only a few names for variables should be used
--     (for example to properly test a-conversion)

type Name = String

data Exp = Var Name
         | Abs Name Exp
         | App Exp Exp
         deriving (Show, Read)

genExp :: Int -> Gen Exp
genExp depth
    | depth <= 1 = Var <$> genNames
    | otherwise = do
        depth1 <- genDepth
        depth2 <- genDepth
        oneof [ genExp 1
              , App <$> genExp depth1 <*> genExp depth2
              , Abs <$> genNames <*> genExp depth1
              ]
  where
    genNames = elements . fmap return $ "aeiou"
    genDepth = elements [1 .. pred depth]

instance Arbitrary Exp where
    arbitrary = sized genExp


