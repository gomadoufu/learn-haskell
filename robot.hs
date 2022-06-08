-- robotオブジェクト
-- Haskellでオブジェクト指向

--{

{-robotコンストラクタ-}
robot (name, attack, hp) = \message -> message (name, attack, hp)

{-アクセサメソッド-}
name (name, _, _) = name

attack (_, attack, _) = attack

hp (_, _, hp) = hp

-- set
setName :: (((a1, b, c) -> ((a2, b, c) -> t1) -> t1) -> t2) -> a2 -> t2
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

setAttack :: (((a, b1, c) -> ((a, b2, c) -> t1) -> t1) -> t2) -> b2 -> t2
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

-- get
getName aRobot = aRobot name

getAtack aRobot = aRobot attack

getHP aRobot = aRobot hp

{-toString-}
printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack:" ++ show a ++ " hp:" ++ show h)

{-damage-}
damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

{-fight-}
fight aRobot defender = damage defender attack
  where
    attack = if getHP aRobot > 10 then getAtack aRobot else 0

{- main (インスタンス作成)-}
killerRobot = robot ("kill3r", 30, 150)

nicerRobot = robot ("nicer", 25, 200)

gentlerRobot = robot ("gentler", 20, 300)
