module GameData where

data Field = Matrix Cell

data Cell = Cell Status

data Status = Dead | Alive
