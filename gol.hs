module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Time = Float
type DeltaT = Float

data World = World {
	settings :: Settings,
	grid :: Grid
}

data Settings = Settings
--data Grid = Node [ Direction -> Node ]
data Grid = Grid 

main = do
	play
		disp
		bgColor
		framerate
		startWorld
		renderWorld
		eventHandler
		moveWorld

disp = undefined
bgColor = undefined

framerate :: Int
framerate = undefined

startWorld :: World
startWorld = undefined

renderWorld :: World -> Picture
renderWorld = undefined

eventHandler :: Event -> World -> World
eventHandler = undefined

moveWorld :: DeltaT -> World -> World
moveWorld world = undefined
