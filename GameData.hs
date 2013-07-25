module GameData where

data Field countX countY = Matrix countY countX Cell

data Cell = Cell Status

data Status = Dead | Alive
