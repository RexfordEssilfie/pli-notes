module TypeclassesAndFunctors where

-- TODO: write me!

class Prettyable a where
  pretty :: a -> String

instance (Prettyable Integer) where
  pretty n = "!!" ++ show n ++ "!!"

data Exp
  = EInt Int
  | EPlus Exp Exp
  deriving (Show, Eq)

instance Prettyable Exp where
  pretty (EInt n) = show n
  pretty (EPlus e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"

prettyExample :: String
prettyExample = pretty (5 :: Integer) ++ " and " ++ pretty (EPlus (EInt 1) (EInt 1))

instance Prettyable a => Prettyable (Maybe a) where
  pretty Nothing  = "{}"
  pretty (Just v) = "{" ++ pretty v ++ "}"

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor Maybe where
--   fmap f Nothing = Nothing
--   fmap f (Just x) = Just $ f x

-- instance Functor [] where
--   fmap f [] = []
--   fmap f (x:xs) = f x :: fmap f xs

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

-- instance Functor ((->) a) where
-- fmap f g = f . g

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   ap :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--   pure = Just
--   ap Nothing _ = Nothing
--   ap _ Nothing = Nothing
--   ap (Just f) (Just x) = Just $ f x

-- instance Applicative [] where
--   pure v = [v]
--   ap fs xs = [f x | f <- fs, x <- xs]