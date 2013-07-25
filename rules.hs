module Judge where

type LivingNeighboursCount = Int
-- |entscheidet je nach Status @a@ der Zelle und ihrer Anzahl
-- lebendiger Nachbarn @b@ ueber den Status im naechsten Zyklus
judge :: Status -> LivingNeighboursCount -> Status
