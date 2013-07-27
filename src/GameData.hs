module GameData where

import Matrix
import Data.Maybe

type Field = Matrix Cell

data Cell = Cell Status
	deriving(Show)

data Status = Dead | Alive
	deriving(Show)

-- creates a new field of dead cells
field width height = fromMaybe (error "field matrix creation failed!") $ m $ [ take width $ repeat (Cell Dead) | i <- [0..(height-1)] ]
