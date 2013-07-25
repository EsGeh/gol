module Main where

import GameData hiding (Field)
import qualified GameData as GD

import Card.Unary

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Time = Float
type DeltaT = Float

type Field = GD.Field N9 N9

data World = World {
	settings :: Settings,
	field :: Field 
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
