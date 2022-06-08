module Queue where

data Queue a = Q [a] [a] deriving (Show)

emptyQueue = Q [] []

singleton x = Q [x] []

enqueue (Q front rear) x = Q front (x : rear)

dequeue (Q [] []) = error "Empty Queue"
dequeue (Q [] rear) = dequeue (Q (reverse rear) [])
dequeue (Q (x : xs) rear) = (x, Q xs rear)

front (Q [] []) = error "Empty Queue"
front (Q [] rear) = front (Q (reverse rear) [])
front (Q (x : _) _) = x

isEmptyQueue (Q [] []) = True
isEmptyQueue (Q _ _) = False
