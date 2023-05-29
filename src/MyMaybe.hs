{-# LANGUAGE InstanceSigs #-}

module MyMaybe where

data MyMaybe a = MyJust a | MyNothing deriving (Show)

instance Functor MyMaybe where
  fmap g f = case f of
    MyNothing -> MyNothing
    (MyJust a) -> MyJust (g a)

instance Applicative MyMaybe where
  pure :: a -> MyMaybe a
  pure = MyJust

  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (<*>) g fun = case g of
    MyNothing -> MyNothing
    (MyJust f) -> case fun of
      MyNothing -> MyNothing
      (MyJust idk) -> MyJust (f idk)

instance Monad MyMaybe where
  (>>=) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  (>>=) m foo = case m of
    MyNothing -> MyNothing
    (MyJust val) -> foo val