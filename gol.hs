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

type PointOnField = MatrIndex -- (Int,Int)
type PointOnScreen = (Float,Float)
type SizeOnScreen = (Float,Float)
type SquareOnScreen = (PointOnScreen,SizeOnScreen)

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

disp = InWindow "GAME OF LIFE!!!" windowPos windowSize
bgColor = black

framerate :: Int
framerate = 40

(windowX,windowY) = (100,100)
windowPos = (windowX,windowY)
(windowWidth,windowHeight) = (800,600)
windowSize = (windowWidth,windowHeight)

(fieldXOnScreen,fieldYOnScreen,fieldWidthOnScreen,fieldHeightOnScreen) = (0,0,400,300)
fieldPos = (fieldXOnScreen,fieldYOnScreen)
fieldSize = (fieldWidthOnScreen,fieldHeightOnScreen)

startWorld :: World
startWorld = World {
	wSettings = Settings,
	wField = field 3 3
}

renderWorld :: World -> Picture
renderWorld world =
	Scale 1 (-1) $	-- flip y axis
	Translate (-(fieldWidthOnScreen)/2) (- (fieldHeightOnScreen)/2) $ -- shift the picture, so that it begins in the upper left corner
	Pictures [ renderBg, renderFields world]

renderBg = Color white $ Polygon path
	where
		path = [
			fieldPos,
			(fieldXOnScreen+fieldWidthOnScreen, fieldYOnScreen),
			(fieldXOnScreen+fieldWidthOnScreen, fieldYOnScreen + fieldHeightOnScreen),
			(fieldXOnScreen, fieldYOnScreen + fieldHeightOnScreen),
			fieldPos
			]


renderFields world = Pictures $ foldToPictureList $ createPictureMatrix field
	where
		field = wField world
		foldToPictureList :: Matrix Picture -> [Picture]
		foldToPictureList matrOfPictures = F.foldr (:) [] matrOfPictures
		createPictureMatrix field' = mapWithIndex (renderCell' field') field'
			where
				renderCell' field'' pos val = renderCell val (squareOnScreenFromPosOnField pos) (viewFromPos field'' pos)
				squareOnScreenFromPosOnField :: PointOnField -> SquareOnScreen
				squareOnScreenFromPosOnField (x,y) = ( (x'*w,y'*h), (w,h))
					where
						(x',y') = (fromIntegral x, fromIntegral y)
				w = fieldWidthOnScreen / (fromIntegral $ mGetWidth field)
				h = fieldHeightOnScreen / (fromIntegral $ mGetHeight field)

-- renders one single cell at a specific position on the field
-- gets a "view" from that position as well.
renderCell :: Cell -> SquareOnScreen -> View -> Picture
renderCell cell ((x,y),(w,h)) view = let
		aliveColor = red
		deadColor = blue
		path = [(x,y),(x,y+h),(x+w,y+h),(x+w,y)]
		cellHeight = fieldHeightOnScreen
	in case cell of
	Cell Dead -> Color deadColor $ Line path
	Cell Alive -> Color aliveColor $ Polygon path -- TO DO: render cell


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
