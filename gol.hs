module Main where

import GameData 
import Matrix
--import qualified GameData as GD


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Time = Float
type DeltaT = Float

data World = World {
	wSettings :: Settings,
	wField :: Field 
}

data Settings = Settings

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
renderWorld = undefined

renderCell :: Cell -> Point -> Picture
renderCell cell position = case cell of
	Cell Dead -> Blank
	Cell Alive -> Blank

eventHandler :: Event -> World -> World
eventHandler = undefined

moveWorld :: DeltaT -> World -> World
moveWorld world = undefined
