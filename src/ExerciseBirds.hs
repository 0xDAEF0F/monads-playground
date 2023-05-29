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