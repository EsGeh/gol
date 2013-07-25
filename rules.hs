module Judge where

-- |entscheidet je nach Status @a@ der Zelle und ihrer Anzahl
-- lebendiger Nachbarn @b@ ueber den Status im naechsten Zyklus
type LivingNeighboursCount = Int
judge :: Status -> LivingNeighboursCount -> Status
