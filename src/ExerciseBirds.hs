module ExerciseBirds () where

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((n + left) - right) > 3 = Nothing
  | otherwise = Just (n + left, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs ((n + right) - left) > 3 = Nothing
  | otherwise = Just (left, n + right)

(-:) a f = f a

keyboards = [10, 20, 30]

drives = [5, 8, 11]

allPairs = keyboards >>= \k -> drives >>= \d -> return (k, d)

allPairs' p = do
  k <- keyboards
  d <- drives
  [(k, d) | p (k, d)]

foo pair = let sum = uncurry (+) pair in sum < 20

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x : xs) = f x (foldr' f acc xs)

foldr1' f xs = foldr f (last xs) (init xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

foldl1' f (x : xs) = foldl' f x xs