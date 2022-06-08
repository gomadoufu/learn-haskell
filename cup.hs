-- cupオブジェクト
-- Haskellでオブジェクト指向

--{
{- cupコンストラクタ コーヒーカップに入っている液体の量を
受け取り、その値が格納されたクロージャを返す -}
cup :: t1 -> (t1 -> t2) -> t2
cup flMl = \message -> message flMl

{- getMl -}
getMl :: ((p -> p) -> t) -> t
getMl aCup = aCup (\flMl -> flMl)

{- setMl -}
setMl :: t1 -> (t1 -> t2) -> t2
setMl newMl = cup newMl

{- drink -}
drink :: (Ord t1, Num t1) => ((p -> p) -> t1) -> t1 -> (t1 -> t2) -> t2
drink aCup mlDrank =
  if mlDiff >= 0
    then cup mlDiff
    else cup 0
  where
    flMl = getMl aCup
    mlDiff = flMl - mlDrank

{- afterManyDrinks -}
afterManyDrinks :: (Ord p, Num p) => ((p -> p) -> p) -> (p -> p) -> p
afterManyDrinks aCup = foldl drink aCup [1, 1, 1, 1, 1]

{- isEmpty -}
isEmpty :: (Eq a, Num a) => ((p -> p) -> a) -> Bool
isEmpty aCup = getMl aCup == 0

{- static main関数-}
coffeeCup :: (Integer -> t2) -> t2
coffeeCup = cup 12

--}
