module Main where

import GameData 
import Rules
import Matrix
import Vector2D
import Prelude hiding(Left,Right)
import qualified Data.Foldable as F
--import qualified GameData as GD


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Up,Down)
import qualified Graphics.Gloss.Interface.Pure.Game as G

type Time = Float
type DeltaT = Float


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

vecFToI :: Vec Float -> Vec Int
vecFToI (x,y) = (floor x, floor y)
vecIToF :: Vec Int -> Vec Float
vecIToF (x,y) = (fromIntegral x, fromIntegral y)

-- aliases for vector functions, to make clear if they operate on a position or a size
pointX = vecX
pointY = vecY
sizeWidth = vecX
sizeHeight = vecY


data World = World {
	wSettings :: Settings,
	wField :: Field 
}

data Settings = Settings {
	paused :: Bool,
	mouseButtonPressed :: Bool
}
settingsStart = Settings False False

-- direction for movements or "pointers" on the field
data Direction = Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft 

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

disp = InWindow "GAME OF LIFE!!!" (windowSize dispSettings) (windowPos dispSettings)
bgColor = black

framerate :: Int
framerate = 1


startWorld :: World
startWorld = World {
	wSettings = settingsStart,
	wField = randomField 50 50 0
}

dispSettings = DisplaySettings {
	windowPos = (100,100),
	windowSize = (800,600),

	fieldPos = (10,10),
	fieldSize = (600,600)
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
	(uncurry Translate) (screenCoordsToMath (0,0)) $
	Scale 1 (-1) $	-- flip y axis
	--Translate ( -(fromIntegral $ sizeWidth $ windowSize dispSettings)/2) (-(fromIntegral $ sizeHeight $ windowSize dispSettings)/2) $ -- shift the picture, so that it begins in the upper left corner
	Pictures [ renderBg, renderField (vecIToF $ fieldPos dispSettings, vecIToF $ fieldSize dispSettings) world]

-- render the background:
renderBg = Color black $ Polygon path
	where
		path = map vecIToF [
			fieldPos dispSettings,
			fieldPos dispSettings <+> (sizeWidth $ fieldSize', 0),
			fieldPos dispSettings <+> fieldSize',
			fieldPos dispSettings <+> (0, sizeHeight $ fieldSize'),
			fieldPos dispSettings
			]
		fieldSize' = fieldSize dispSettings

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
				squareOnScreenFromPosOnField fieldPos = ( posBase <+> (vecIToF fieldPos) <*> size, size)
					where
						posBase = pointFromSquare square
						size = (sizeFromSquare square) </> (vecIToF $ (mGetWidth field, mGetHeight field))

-- renders one single cell at a specific position on the field
-- gets a "view" from that position as well.
renderCell :: Cell -> SquareOnScreen -> View -> Picture
renderCell cell ((x,y),(w,h)) view = let
		aliveColor = red
		deadColor = blue --mixColors 0.0 1.0 blue black
		path = [(x,y),(x,y+h),(x+w,y+h),(x+w,y)]
	in case cell of
	Cell Dead -> Color deadColor $ Line path
	Cell Alive -> Color aliveColor $ Polygon path -- TO DO: render cell


eventHandler :: Event -> World -> World
eventHandler event world@World{ wSettings=settingsOld } = case event of
	EventKey (SpecialKey KeySpace) G.Down _ _ ->
		-- trigger pause:
		world{ wSettings=settingsOld{ paused = not $ paused $ settingsOld} } --where settingsOld = wSettings world
	EventKey (MouseButton button) downUp _ (x,y) -> 
		let settingsOld = wSettings world
		in case downUp of
			G.Down -> callMouseMoveOnce $ world{ wSettings=settingsOld{ mouseButtonPressed=True } }
				where callMouseMoveOnce world = eventHandler (EventMotion (x,y)) world
			G.Up -> world{ wSettings=settingsOld{ mouseButtonPressed=False } }
	EventMotion (x,y) -> case mouseButtonPressed settingsOld of
		False -> world
		True ->
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
moveWorld deltaT oldWorld@World{ wField=oldField } = oldWorld { wField=newField oldField}
	where
		newField oldField = case (paused $ wSettings $ oldWorld) of
			True -> oldField
			False -> mapWithIndex (moveCell oldField) oldField
				where
					moveCell :: Field -> MatrIndex -> Cell -> Cell
					moveCell field index (Cell status) = Cell $ judge status livingNeighboursCount
						where
							livingNeighboursCount = sum $ map (numFromStatus . viewFromPos field index) [Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft]
							numFromStatus s = case s of
								(Cell Alive) -> 1
								(Cell Dead) -> 0


----------------------------------------------------------------------------------
-- useful helper functions:
----------------------------------------------------------------------------------

-- creates a "view" from a position on the field
viewFromPos :: Field -> PointOnField -> View
viewFromPos field pos dir = mGet (getNeighbourIndex field pos dir) field 

-- realizes a "torus like" behavior for positions on the field
getNeighbourIndex :: Field -> PointOnField -> Direction -> PointOnField
getNeighbourIndex field pos@(x,y) dir = case dir of
	Up -> (x,(y-1) `niceMod` height)
	UpRight -> getNeighbourIndex field (getNeighbourIndex field pos Up) Right
	Right -> ((x+1) `niceMod` width, y)
	DownRight -> getNeighbourIndex field (getNeighbourIndex field pos Down) Right
	Down -> (x,(y+1) `niceMod` height)
	DownLeft -> getNeighbourIndex field (getNeighbourIndex field pos Down) Left
	Left -> ((x-1) `niceMod` width, y)
	UpLeft -> getNeighbourIndex field (getNeighbourIndex field pos Up) Left
	where
		width = mGetWidth field
		height = mGetHeight field
		niceMod val m = case signum val of
			(-1) -> niceMod (val+m) m
			(1) -> val `mod` m
			(0) -> 0
			otherwise -> error "niceMod internal error!"

-- takes a position on the screen (e.g. mouse pointer) and calculates a position on the field (0..1, 0..1)
screenPosToFieldPos :: PointOnScreen -> FloatPosOnField
screenPosToFieldPos screenPos = divideByBoardSize $ mathToScreenCoords screenPos <-> (vecIToF $ fieldPos dispSettings)
	where
		divideByBoardSize point = point </> (vecIToF $ fieldSize dispSettings)

screenCoordsToMath :: PointOnScreen -> PointOnScreen
screenCoordsToMath screenPos = (screenPos <-> ( vecIToF $ windowSize dispSettings) </ 2 ) <*> (1,-1)

mathToScreenCoords :: PointOnScreen -> PointOnScreen
mathToScreenCoords screenPos = (screenPos <*> (1,-1)) <+> (vecIToF $ windowSize dispSettings) </ 2
