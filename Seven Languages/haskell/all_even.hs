module Main where

-- *Main> [2 * x | x <- [1..10]]
-- [2,4,6,8,10,12,14,16,18,20]
--
-- *Main> [(4 - x, y) | (x, y) <- [(1, 2), (3, 4), (5, 6)]]
-- [(3,2),(1,4),(-1,6)]
--
-- *Main> let crew = ["Kirk", "Spock", "McCoy"]
-- *Main> [(a, b) | a <- crew, b <- crew]
-- [("Kirk","Kirk"),("Kirk","Spock"),("Kirk","McCoy"),("Spock","Kirk"),
--   ("Spock","Spock"),("Spock","McCoy"),("McCoy","Kirk"),("McCoy","Spock"),
--   ("McCoy","McCoy")]
--
-- *Main> [(a, b) | a <- crew, b <- crew, a /= b]
-- [("Kirk","Spock"),("Kirk","McCoy"),("Spock","Kirk"),("Spock","McCoy"),
--   ("McCoy","Kirk"),("McCoy","Spock")]
--
-- *Main> [(a, b) | a <- crew, b <- crew, a < b]
-- [("Kirk","Spock"),("Kirk","McCoy"),("McCoy","Spock")]

allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:t) =
  if even h then
    h:allEven t
  else
    allEven t
