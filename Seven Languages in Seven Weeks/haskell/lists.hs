module Main where

second = head . tail

size [] = 0
size (_:t) = 1 + size t

sum2 [] = 0
sum2 (h:t) = h + sum2 t

prod [] = 1
prod (h:t) = h * prod t
