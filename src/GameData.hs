module GameData where

import Matrix
import Data.Maybe
import System.Random
import Control.Monad.State
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid

type Field = Matrix Cell

data Cell = Cell Status
	deriving(Show)

data Status = Dead | Alive
	deriving(Show)
instance Random Status where
	randomR range gen = fromBoolTuple $ randomR (toBoolRange range) gen
		where
			toBoolRange (l,h) = (toBool l, toBool h)
			fromBoolTuple (x,g) = (fromBool x,g)

toBool status = case status of
	Dead -> False
	Alive -> True
fromBool bool = case bool of
	False -> Dead
	True -> Alive

-- creates a new field of dead cells
field :: Width -> Height -> Field
field width height = fromMaybe (error "field matrix creation failed!") $ m $ [ take width $ repeat (Cell Dead) | i <- [0..(height-1)] ]

randomField :: Int -> Int -> Int -> Matrix Cell --State StdGen Int
randomField width height seed = fmap (Cell . fromBool) $ snd $ T.mapAccumL combine (mkStdGen seed) (fmap (const randomSt) $ field width height)
	where
		combine :: (Random t) => StdGen -> State StdGen t -> (StdGen, t)
		s `combine` rnd = (\(f,s) -> (s,f)) $ runState rnd $ s

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random
