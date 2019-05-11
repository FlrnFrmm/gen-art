module Recaman where

recaman :: Int -> [Int]
recaman max
  | max > 0 = foldl (\x y -> nextRecamanNumber x) [0] [1..max]
  | otherwise = []

nextRecamanNumber :: [Int] -> [Int]
nextRecamanNumber vs 
  | (next > 0) && and (map (\v -> v /= next) vs) = vs ++ [next]
  | otherwise = vs ++ [((last vs) + n)]
  where
    n = length vs
    next = (last vs) - n

type Point = (Float, Float)

recamanAsPoints :: Int -> [Point]
recamanAsPoints count = map (\p -> (0, fromIntegral p)) points
  where
    points = recaman count

translate :: Point -> Point -> Point
translate offset point = ((fst offset) + (fst point), (snd offset) + (snd point))

scale :: Point -> Point -> Point
scale factor point = ((fst factor) * (fst point), (snd factor) * (snd point))

rotate :: Float -> Point -> Point
rotate angle point = (0,0)