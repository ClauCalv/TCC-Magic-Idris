import Data.Vect

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in zipWith (::) x xsTrans

multLine : Num a => Vect (S m) a -> Vect p (Vect (S m) a) -> Vect p a
multLine xs [] = []
multLine xs (ys :: yss) = sum (zipWith (*) xs ys) :: multLine xs yss

multMatrix' : Num a => Vect n (Vect (S m) a) -> Vect (S p) (Vect (S m) a) -> Vect n (Vect (S p) a)
multMatrix' [] yss = []
multMatrix' (xs :: xss) yss = multLine xs yss :: multMatrix' xss yss

multMatrix : Num a => Vect (S n) (Vect (S m) a) -> Vect (S m) (Vect (S p) a) -> Vect (S n) (Vect (S p) a)
multMatrix m1 m2 =  let m2' = transposeMat m2 in multMatrix' m1 m2'

data Shape = Triangle Double Double
| Rectangle Double Double
| Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius
