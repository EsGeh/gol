{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Matrix(
	Matrix(),
	MatrIndex,IndexRow,IndexCol,
	mGetAllIndexRow,mGetAllIndexCol,mGetAllIndex,
	m,mSqr,mGetHeight,mGetWidth,mGet,
	WithOriginMatr,WithLog,Log(..),Origin(..),LogVal(..),
	mGetWithOrigin 
	) where
--import Card as Unary
import qualified PrettyShow as Pretty

import Data.Foldable hiding(concat,toList)
import Data.Foldable as Fold hiding(concat,toList)
import Data.List hiding(foldl,foldr)
--import qualified Data.List as List
import Prelude hiding(foldl,foldr,Left,Right)
import Data.Monoid 
import Data.Maybe
import Control.Monad.Writer
import Data.Ratio
import Data.Array


data Matrix t = M (Array Int (Array Int t))
--data Matrix t = M (CountingList m (CountingList n t))
-- instances
instance (Show t) => Show (Matrix t) where
	show m@(M listLines) = 
		concat $ intersperse "\n" $ elems $ fmap (prettyShow " | " ((fromIntegral maxLength)%1) 0 ) $ listLines
			where
				maxLength = Fold.maximum $ fmap (length . show) m
				prettyShow = Pretty.showContainer "" "" " " " " Pretty.LeftJust
instance Functor Matrix where
	fmap f (M listLines) = M $ fmap (fmap f) listLines
instance Foldable Matrix where
	foldMap toMonoid (M l) = foldMap (foldMap toMonoid) l

arrayFromList :: Int -> [a] -> Array Int a
arrayFromList length list = mkArray length (\i -> list !! i)

mkArray :: Int -> (Int -> a) -> Array Int a
mkArray length f = array (0,length-1) [ (i,f i) | i<-[0..(length-1)] ]

-- function
m :: [[t]] -> Maybe (Matrix t)
--m listLines = M $ fromList countLines (map (fromList countCol) listLines)
m listLines = if (isValid listLines)
	then Just $ M $ arrayFromList height $ map (arrayFromList width) listLines
	else Nothing
	where
		isValid listLines = foldl (\x y -> x && (length y==width)) True listLines
		height = length listLines
		width = length $ listLines !! 0

mGetHeight :: Matrix t -> Int
mGetHeight (M listLines) = (+1) $ snd $ bounds $ listLines
mGetWidth :: Matrix t -> Int
mGetWidth m@(M listLines) = if (mGetHeight m > 0) then ((+1) $ snd $ bounds $ listLines ! 0) else 0


mSqr = m

mGet :: MatrIndex -> Matrix t -> t
mGet index (M listLines) = (listLines ! row) ! col
	where
		row = fst index; col = snd index

mIndex m n = (m,n)

mGetWithOrigin :: MatrIndex -> Matrix t -> WithOriginMatr t
mGetWithOrigin index matr = do
	tell $ Log [(ValO index)]
	return $ val
		where val = mGet index matr
mGetAllIndexRow matr = [0..(mGetHeight matr -1)]
mGetAllIndexCol matr = [0..(mGetWidth matr -1)]
mGetAllIndex matr = [(row,col) | row <- mGetAllIndexRow matr, col <- mGetAllIndexCol matr ]

--mGetAllIndexDist matr = [ index | index <- mGetAllIndex matr, (fst index /= snd index) ]


--mGetAllWC matr = [ mGetWC m n matr | m <- [0..(mGetHeight matr - 1)], n <- [0..(mGetWidth matr - 1)] ]

type MatrIndex = (IndexRow,IndexCol)
type IndexRow = Int
type IndexCol = Int
type WithOriginMatr t = Writer (Log (Origin MatrIndex)) t
type WithLog t = Writer (Log (LogVal t)) t
--type WC w t = (Writer w t)

newtype Log logType = Log { getLog :: [logType] }
	deriving(Monoid)
instance (Show logType) => Show (Log logType) where
	show (Log list) = foldl conc "" $ map show list
		where
			conc "" y = y
			conc x y = x ++ " -> " ++ y

data Origin t = NilO | ValO t 
instance (Show t) => Show (Origin t) where
	show NilO = ""
	show (ValO val) = show val

data LogVal t = NilL | ValL t | Fun String
instance (Show t) => Show (LogVal t) where
	show NilL = ""
	show (ValL val) = show val
	show (Fun str) = str