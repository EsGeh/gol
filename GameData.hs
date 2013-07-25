module GameData where

import Matrix

data Field = Field (Matrix Cell)

data Cell = Cell Status

data Status = Dead | Alive

-- creates a new field of dead cells
field width height = Field $ fromMaybe (error "field matrix creation failed!") $ m $ [ take width $ repeat (Cell Dead) | i <- [0..(height-1)] ]
