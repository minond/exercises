module Lib where

import Prelude

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data MyList a
  = MyNil
  | MyCons a (MyList a)
  deriving (Show)

myHead (MyNil) = Nothing
myHead (MyCons head _) = Just head
