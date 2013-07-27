module Main where

import GameData 
import Matrix
import Prelude hiding(Left,Right)
import qualified Data.Foldable as F
--import qualified GameData as GD


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Up,Down)
import qualified Graphics.Gloss.Interface.Pure.Game as G

type Time = Float
type DeltaT = Float

type Vec a = (a,a)

-- the position on the field.
-- it serves a index for the matrix representing it
type PointOnField = MatrIndex -- (Int,Int)

-- the following types represent things in screen coordinates (as opposed to positions on the Field!):
type PointOnScreen = Vec Float -- (Float,Float)
type SizeOnScreen = Vec Float -- (Float,Float)
type SquareOnScreen = (PointOnScreen,SizeOnScreen)

sizeFromSquare :: SquareOnScreen -> SizeOnScreen
sizeFromSquare (pos,size) = size
pointFromSquare :: SquareOnScreen -> PointOnScreen
pointFromSquare (pos,size) = pos

-- two values between 0..1
-- describes the position on the field.
-- 	(0,0) means left upper corner
-- 	(1,1) means right bottom corner
type FloatPosOnField = (Float,Float)

-- some useful functions for working with vectors:
vecX :: Vec a -> a
vecX (x,y) = x
vecY :: Vec a -> a
vecY (x,y) = y

vecFToI :: Vec Float -> Vec Int
vecFToI (x,y) = (floor x, floor y)
vecIToF :: Vec Int -> Vec Float
vecIToF (x,y) = (fromIntegral x, fromIntegral y)

-- aliases for vector functions, to make clear if they operate on a position or a size
pointX = vecX
pointY = vecY
sizeWidth = vecX
sizeHeight = vecY

infix 8 <+> -- vector addition
infix 8 <-> -- vector subtraction
infix 9 <*> -- component-by-component multiplication (!)
infix 9 </> -- component-by-component division (!)
infix 9 *> -- scalar mult
infix 9 <* -- scalar mult
infix 9 /> -- scalar div
infix 9 </ -- scalar div
--(:+:) :: (Num a) => Vec a -> Vec a -> Vec a
l <+> r = (pointX l + pointX r,  pointY l + pointY r)
l <-> r = (pointX l - pointX r,  pointY l - pointY r)
l <*> r = (pointX l * pointX r,  pointY l * pointY r)
l </> r = (pointX l / pointX r,  pointY l / pointY r)
scalar *> vec = (scalar * (pointX vec), scalar * (pointY vec))
(<*) = flip (*>)
scalar /> vec = (scalar / (pointX vec), scalar / (pointY vec))
vec </ scalar = ((pointX vec) / scalar, (pointY vec) / scalar)

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


-- some information needed for rendering:
data DisplaySettings = DisplaySettings {
	windowPos :: (Int,Int),
	windowSize :: (Int,Int),

	fieldPos :: (Int,Int),
	fieldSize :: (Int,Int)
}


----------------------------------------------------------------------------------
-- some important settings:
----------------------------------------------------------------------------------

disp = InWindow "GAME OF LIFE!!!" (windowPos dispSettings) (windowSize dispSettings)
bgColor = black

framerate :: Int
framerate = 40


startWorld :: World
startWorld = World {
	wSettings = Settings,
	wField = field 3 3
}

dispSettings = DisplaySettings {
	windowPos = (100,100),
	windowSize = (800,600),

	fieldPos = (0,0),
	fieldSize = (400,300)
}


----------------------------------------------------------------------------------
-- main :
----------------------------------------------------------------------------------

main = do
	play
		disp
		bgColor
		framerate
		startWorld
		renderWorld
		eventHandler
		moveWorld


----------------------------------------------------------------------------------
-- functions
----------------------------------------------------------------------------------

-- uses "renderBg" and "renderField"
renderWorld :: World -> Picture
renderWorld world =
	Scale 1 (-1) $	-- flip y axis
	Translate ( -(fromIntegral $ sizeWidth $ windowSize dispSettings)/2) (-(fromIntegral $ sizeHeight $ windowSize dispSettings)/2) $ -- shift the picture, so that it begins in the upper left corner
	Pictures [ renderBg, renderField (vecIToF $ fieldPos dispSettings, vecIToF $ fieldSize dispSettings) world]

-- render the background:
renderBg = Color white $ Polygon path
	where
		path = [
			(fieldXOnScreen, fieldYOnScreen),
			(fieldXOnScreen+fieldWidthOnScreen, fieldYOnScreen),
			(fieldXOnScreen+fieldWidthOnScreen, fieldYOnScreen + fieldHeightOnScreen),
			(fieldXOnScreen, fieldYOnScreen + fieldHeightOnScreen),
			(fieldXOnScreen, fieldYOnScreen)
			]
		(fieldXOnScreen, fieldYOnScreen) = vecIToF $ fieldPos dispSettings
		(fieldWidthOnScreen, fieldHeightOnScreen) = vecIToF $ fieldSize dispSettings

-- render the field using renderCell:
renderField :: SquareOnScreen -> World -> Picture
renderField square world = Pictures $ foldToPictureList $ createPictureMatrix field
	where
		field :: Field -- remember "Field" is defined as "Matrix Cell"
		field = wField world

		foldToPictureList :: Matrix Picture -> [Picture]
		foldToPictureList matrOfPictures = F.foldr (:) [] matrOfPictures

		createPictureMatrix :: Matrix Cell -> Matrix Picture
		createPictureMatrix field' = mapWithIndex (renderCell' field') field'
			where
				renderCell' field'' pos val = renderCell val (squareOnScreenFromPosOnField pos) (viewFromPos field'' pos)
				squareOnScreenFromPosOnField :: PointOnField -> SquareOnScreen
				squareOnScreenFromPosOnField (x,y) = ( (x'*w,y'*h), (w,h))
					where
						(x',y') = vecIToF (x, y)
				w = (fromIntegral $ sizeWidth $ fieldSize dispSettings) / (fromIntegral $ mGetWidth field)
				h = (fromIntegral $ sizeHeight $ fieldSize dispSettings) / (fromIntegral $ mGetHeight field)

-- renders one single cell at a specific position on the field
-- gets a "view" from that position as well.
renderCell :: Cell -> SquareOnScreen -> View -> Picture
renderCell cell ((x,y),(w,h)) view = let
		aliveColor = red
		deadColor = blue
		path = [(x,y),(x,y+h),(x+w,y+h),(x+w,y)]
		--cellHeight = fieldHeightOnScreen
	in case cell of
	Cell Dead -> Color deadColor $ Line path
	Cell Alive -> Color aliveColor $ Polygon path -- TO DO: render cell


eventHandler :: Event -> World -> World
eventHandler event world = case event of
	EventKey (MouseButton button) G.Down _ (x,y) ->
		-- set one cell to "Alive":
		world {
			wField = mSet fieldPos (Cell Alive) $ field 
		}
			where
				field = wField world
				fieldPos = ( floor $ x' * (fromIntegral fieldW),  floor $ y' * (fromIntegral fieldH))
				(x',y') = screenPosToFieldPos (x,y)
				(fieldW, fieldH) = (mGetWidth field, mGetHeight field)
	otherwise -> world

-- update world
moveWorld :: DeltaT -> World -> World
moveWorld deltaT = id -- TO DO: calculate world


----------------------------------------------------------------------------------
-- useful helper functions:
----------------------------------------------------------------------------------

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

-- takes a position on the screen (e.g. mouse pointer) and calculates a position on the field (0..1, 0..1)

screenPosToFieldPos :: PointOnScreen -> FloatPosOnField
screenPosToFieldPos screenPos = divideByBoardSize $ mathToScreenCoords screenPos
	where
		divideByBoardSize point = point </> (vecIToF $ fieldSize dispSettings)

mathToScreenCoords :: PointOnScreen -> PointOnScreen
mathToScreenCoords screenPos = (screenPos <*> (1,-1)) + (vecIToF $ windowSize dispSettings) </ 2