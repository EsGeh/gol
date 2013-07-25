module GameData where

import Matrix

data Field = Field (Matrix Cell)

data Cell = Cell Status

data Status = Dead | Alive
