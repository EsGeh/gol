module Main where

import GameData 
import Matrix
import Prelude hiding(Left,Right)
import qualified Data.Foldable as F
--import qualified GameData as GD


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Up,Down)

type Time = Float
type DeltaT = Float

type PointOnField = (Int,Int)
type PointOnScreen = Point

data World = World {
	wSettings :: Settings,
	wField :: Field 
}

data Settings = Settings

-- direction for movements or "pointers" on the field
data Direction = Up | Down | Left | Right

-- this is what the render function gets so that it knows about the
-- neighbour cells:
type View = Direction -> Cell

main = do
	play
		disp
		bgColor
		framerate
		startWorld
		renderWorld
		eventHandler
		moveWorld

disp = InWindow "GAME OF LIFE!!!" (800,600) (100,100)
bgColor = black

framerate :: Int
framerate = 40

startWorld :: World
startWorld = World {
	wSettings = Settings,
	wField = field 20 20
}

renderWorld :: World -> Picture
renderWorld world = Pictures $ foldToPictureList $ createPictureMatrix field
	where
		field = wField world
		foldToPictureList :: Matrix Picture -> [Picture]
		foldToPictureList matrOfPictures = F.foldr (:) [] matrOfPictures
		createPictureMatrix field' = mapWithIndex (renderCell' field') field'
			where
				renderCell' field pos val = renderCell val pos (viewFromPos field pos)


-- renders one single cell at a specific position on the field
-- gets a "view" from that position as well.
renderCell :: Cell -> PointOnField -> View -> Picture
renderCell cell position view = case cell of
	Cell Dead -> Blank
	Cell Alive -> Blank -- TO DO: render cell

eventHandler :: Event -> World -> World
eventHandler event = id -- TO DO: handle events

moveWorld :: DeltaT -> World -> World
moveWorld deltaT = id -- TO DO: calculate world


-- creates a "view" from a position on the field
viewFromPos :: Field -> PointOnField -> View
viewFromPos field pos dir = mGet (moveIndex field pos dir) field 

-- realizes a "torus like" behavior for positions on the field
moveIndex :: Field -> PointOnField -> Direction -> PointOnField
moveIndex field (x,y) dir = case dir of
	Up -> (x,(y-1) `niceMod` height)
	Down -> (x,(y+1) `niceMod` height)
	Left -> ((x-1) `niceMod` width, y)
	Right -> ((x+1) `niceMod` width, y)
	where
		width = mGetWidth field
		height = mGetHeight field
		niceMod val m = case signum val of
			(-1) -> niceMod (val+m) m
			(1) -> val `mod` m
