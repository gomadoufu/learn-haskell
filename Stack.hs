module Stack where

data Stack a = S [a] deriving (Show)

emptyStack = S []

singleton x = S [x]

push :: Stack a -> a -> Stack a
push (S xs) x = S (x : xs)

pop (S []) = error "Empty Stack"
pop (S (x : xs)) = (x, S xs)

top (S []) = error "Empty Stack"
top (S (x : _)) = x

isEmptyStack (S []) = True
isEmptyStack (S _) = False
