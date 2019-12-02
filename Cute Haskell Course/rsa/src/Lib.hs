module Lib where

-- Steps:
-- 1. Create 2 very lange prime numbers (p, q)
-- 2. Multiply them togeter to get n
-- 3. Create a number which is relatively prime to (p - 1) * (p - 1) = e
-- 4. Get the modular inverse of e and store that in d
-- 5. The private key will be the tuple (n, e)
-- 6. The public key will be the tuple (n, d)
--
-- General questions:
-- encrypt: C = M ^ e mod n
-- decrypt: M = C ^ d mod n
--
--
-- Block encoding:
-- - Cacluclte block size - for security must satisfy 2 ^ Key Size > Symbol Set
--   Size ^ Block Size. Fos us this will be 169: (2 ^ 1024 > 66 ^ 169)
-- - Split text into sublists of characters of block size
--
--   -> Hello World                               Block size 3
--   -> [H, e, l], [l, o, ], [W, o, r], [l, d]
--   -> [H, e, l, o, W, r, d]                     8 allowed characters
--
-- - Convert each character in each list to a number by calculating (p * s ^ i)
--   where p is the position of the character in the symbol set, s is the size
--   of the symbol set and i is the index of the character in the sublist.
--
--   -> [0 * 8 ^ 0, 1 * 8 ^ 1, 2 * 8 ^ 2], ...
--
-- - Sum all the numbers

someFunc :: IO ()
someFunc = putStrLn "someFunc"
