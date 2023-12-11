module Main where

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
            Branch l r -> Either [] (Branch (l >>= f) (r >>= f))

main :: IO ()
main = do
    putStrLn "lab 3"

