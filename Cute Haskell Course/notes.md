# Cute Haskell Crash Course

## Primitives

### Functions

```haskell
\a b c -> a + b + c
```

```haskell
addThree a b c = a + b + c
```

## Types

```haskell
addThreeThings :: Eq a => a -> a -> a -> Bool
addThreeThings a b c = (a == b) && (b == c)
```


## TypeClasses

Way to share common API of funtions across different types.

```haskell
Prelude> :t show
show :: Show a => a -> String
```

`Show` is a TypeClass.

```haskell
data MyType = AType deriving Show

x = AType

show x


data MyType2 = AType2 Integer | BType2 String deriving (Show, Eq)

y = AType2 123
z = BType2 "123"

show y == z
show z

nothin1 :: AType2 -> Integer
nothin1 (AType2 i) = i
```
