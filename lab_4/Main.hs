module Main where

import Test.QuickCheck
import Control.Monad

import Data.List (isPrefixOf)

------------------------------------------------------------------------------------------
-----------------------------------    lab 3    ------------------------------------------
------------------------------------------------------------------------------------------

data ErrorTree a = Either [a] (AuxTree a)
data AuxTree   a = Leaf a 
                 | Branch (ErrorTree a) (ErrorTree a)

instance Functor ErrorTree
    where 
        -- fmap :: (a -> b) -> ErrorTree a -> ErrorTree b
        fmap f (Either errs tree) = Either (f<$>errs) (f<$>tree)

instance Functor AuxTree
    where
        -- fmap :: (a -> b) -> AuxTree a -> AuxTree b
        fmap f (Leaf a) = Leaf (f a)
        fmap f (Branch l r) = Branch (f <$> l) (f <$> r) 

instance Applicative AuxTree 
    where
        -- pure :: a -> AuxTree a
        pure = Leaf
        -- (<*>) :: AuxTree (a -> b) -> AuxTree a -> AuxTree b
        (<*>) (Leaf f) t = fmap f t 
        (<*>) (Branch lf rf) (Branch lv rv) = Branch (lf <*> lv) (rf <*> rv)

instance Applicative ErrorTree 
    where
        -- pure :: a -> ErrorTree a
        pure x = Either [] (pure x)
        -- (<*>) :: ErrorTree (a -> b) -> ErrorTree a -> ErrorTree b
        (<*>) (Either ef f) (Either ex x) = Either (ef <*> ex) (f <*> x)   


instance Monad ErrorTree
    where
        -- return :: a -> ErrorTree a
        return = pure
        -- (>>=) :: ErrorTree a -> (a -> ErrorTree b) -> ErrorTree b
        (>>=) (Either errs tree) f = case tree of 
            Leaf x     -> f x
            Branch l r -> Either 
                            (concatMap  ( (\( Either errs tree) -> errs ) . f) errs) 
                            (Branch (l >>= f) (r >>= f))

------------------------------------------------------------------------------------------
------------------------------    end of lab 3    ----------------------------------------
------------------------------------------------------------------------------------------


instance (Eq a) => Eq (ErrorTree a) where
--   (==) :: ErrorTree a -> ErrorTree a -> Bool
  (Either e1 tree1) == (Either e2 tree2) = tree1 == tree2 && e1 == e2

instance (Eq a) => Eq (AuxTree a) where
--   (==) :: AuxTree a -> AuxTree a -> Bool
  (Leaf l)       == (Leaf r)       = l == r  
  (Branch l1 r1) == (Branch l2 r2) = l1 == l2 && r1 == r2
  _              == _              = False

instance (Ord a) => Ord (ErrorTree a) where
--   compare :: Ord a => ErrorTree a -> ErrorTree a -> Ordering
  (Either e1 tree1) `compare` (Either e2 tree2) = tree1 `compare` tree2

instance (Ord a) => Ord (AuxTree a) where
--   compare :: Ord a => AuxTree a -> AuxTree a -> Ordering
  (Leaf l)       `compare` (Leaf r)       = l `compare` r 
  (Leaf _)       `compare` _              = LT
  _              `compare` (Leaf _)       = GT 
  (Branch l1 r1) `compare` (Branch l2 r2) = case l1 `compare` l2 of
                                                 EQ  -> r1 `compare` r2
                                                 ord -> ord

instance Foldable ErrorTree
    where 
        -- foldr :: (a -> b -> b) -> b -> ErrorTree a -> b
        foldr f z (Either errs tree) = foldr f z tree
 
instance Foldable AuxTree
    where 
        -- foldr :: (a -> b -> b) -> b -> AuxTree a -> b
        foldr f z (Leaf x) = f x z 
        foldr f z (Branch l r) = foldr f (foldr f z l) r


instance (Arbitrary a) => Arbitrary (AuxTree a) 
  where
    arbitrary :: Gen (AuxTree a)
    arbitrary = sized tree'
      where 
        tree' 0 = liftM Leaf arbitrary
        tree' n | n>0 = 
          oneof [liftM Leaf arbitrary,
                  liftM2 Branch arbitrary arbitrary]


instance (Arbitrary a) => Arbitrary (ErrorTree a)
  where
    arbitrary :: Gen (ErrorTree a)
    arbitrary = do
      errs <- arbitrary
      Either errs <$> arbitrary

prop_foldr_ErrorTree :: Blind (Int -> Int -> Int) -> Int -> ErrorTree Int -> Bool
prop_foldr_ErrorTree (Blind f) z errorTree = case errorTree of
    Either _ auxTree -> foldr f z errorTree == foldr f z auxTree

prop_foldr_AuxTree :: Blind (Int -> Int -> Int) -> Int -> AuxTree Int -> Bool
prop_foldr_AuxTree (Blind f) z auxTree = case auxTree of
    Leaf x -> f x z == foldr f z auxTree
    Branch l r -> case (l, r) of
      (Either _ ll, Either _ rr) -> do
                let leftFoldr = foldr f z l
                foldr f leftFoldr r == foldr f z auxTree 
                            && prop_foldr_AuxTree (Blind f) z ll
                            && prop_foldr_AuxTree (Blind f) leftFoldr rr

instance (Show a) => Show (ErrorTree a) where
--   show :: Show a => ErrorTree a -> String
  show (Either errs tree) = "Tree " ++ show errs ++ " (" ++ show tree ++ ")"

instance (Show a) => Show (AuxTree a) where
--   show :: Show a => AuxTree a -> String
  show (Leaf x) = "L " ++ show x
  show (Branch l r) = "B (" ++ show l ++ ")(" ++ show r ++ ")"

instance (Read a) => Read (ErrorTree a) where
  readsPrec _ input = parse errorTreeParser input

instance (Read a) => Read (AuxTree a) where
  readsPrec _ input = parse auxTreeParser input

newtype Parser a = P (String -> [(a,String)])
parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

instance Functor Parser 
  where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p1) = P (\s -> map (\(val, s) -> (f val, s))  (p1 s))

instance Applicative Parser 
  where
  -- pure :: a -> Parser a
  pure x = P (\s -> [(x, s)])
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\inp -> case parse pf inp of
    [] -> []
    [(g,out)] -> parse (fmap g px) out)  

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Alternative Parser 
  where
    empty = P (const [])
    px <|> py = P (\s -> parse px s ++ parse py s)

prefixP :: String -> Parser String
prefixP s = P f
  where
    f input = if s `isPrefixOf` input
                then [(s, drop (length s) input)]
                else []

skipString :: String -> Parser ()
skipString s = () <$ prefixP s

listParser :: Read a => Parser [a]
listParser = P $ \s -> readList s

defaultParser :: Read a => Parser a
defaultParser = P $ \s -> reads s

errorTreeParser :: Read a => Parser (ErrorTree a)
errorTreeParser = Either <$> (skipString "Tree " *> listParser) <*> (skipString " (" *> auxTreeParser <* skipString ")")

auxTreeParser :: Read a => Parser (AuxTree a)
auxTreeParser = leafParser <|> branchParser
  where
      leafParser = Leaf <$> (skipString "L " *> defaultParser)
      branchParser = Branch <$> (skipString "B (" *> errorTreeParser) <*> (skipString ")(" *> errorTreeParser <* skipString ")")

-- use Int to print the result
parseErrorTree :: String -> Parser (ErrorTree Int) -> Maybe (ErrorTree Int)
parseErrorTree s (P p1) = case p1 s of
  [(val, "")] -> Just val
  _ -> Nothing

main :: IO ()
main = do
    putStrLn "lab 4"

    putStrLn "Show ErrorTree:"
    let myTree = Either [] (Branch (Either [1, 4] (Leaf 2)) (Either [1] (Leaf 3))) 
    print myTree
    
    print $ parseErrorTree "Tree [1] (L 4)" errorTreeParser
    print (reads "Tree [] (B (Tree [2, 4, 5, 6] (L 3))(Tree [] (L 2)))end" :: [(ErrorTree Int, String)])
    
    putStrLn "QuickCheck foldr of ErrorTree:"
    quickCheck prop_foldr_ErrorTree
    putStrLn "QuickCheck foldr of AuxTree:"
    quickCheck prop_foldr_AuxTree

