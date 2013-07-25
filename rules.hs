module Judge where

import Cell

-- |entscheidet je nach Status @a@ der Zelle und ihrer Anzahl
-- lebendiger Nachbarn @b@ ueber den Status im naechsten Zyklus
type LivingNeighboursCount = Int
judge :: Status -> LivingNeighboursCount -> Status
judge Dead count
    | (count == 3) = Alive
    | otherwise = Dead
judge Alive count 
    | (count == 2 || count == 3) = Alive
    | otherwise = Dead
