module MHLib where

import MHDataTypes


mkExtremes :: Double -> Extremes
mkExtremes a = Ex a a

rcExtremes :: Extremes -> Double -> Extremes
rcExtremes e v =  (mkExtremes v) `mappend` e

mkElements :: Elems -> Elements
mkElements els = Elements els len acc
  where lambda = (\e (len, acc) -> (len+1, acc+e))
        (len, acc) = foldr lambda (0, 0) els

elementsAverage :: Elements -> Average
elementsAverage (Elements _ l a ) = a / (fromIntegral l)

elementsDiffAccExtremes :: Elements -> DiffAccExtremes
elementsDiffAccExtremes e@(Elements els _ _) = ext
  where avg = elementsAverage e
        ext = snd $ foldr (\ce (acc, ex) -> let a = (ce - avg) + acc
                                                in (a, rcExtremes ex a)) (0, Inv) els
elementsTreRange :: Elements -> TresholdRange
elementsTreRange es =
  case (elementsDiffAccExtremes es) of
    (Ex a b) -> b - a
    _ -> 0

elementsDeviation :: Elements -> Deviation
elementsDeviation e@(Elements es l _) = sqrt (ac / (fromIntegral l) )
  where avg = elementsAverage e
        ac = foldr (\a acc -> ( (** 2) (a - avg) + acc)) 0 es

mkHDT :: Elements -> HurstDataType
mkHDT es@(Elements _ l _ ) = HDT es tr dv pt
  where tr = elementsTreRange es
        dv = elementsDeviation es
        pt = hdtCalcPoint tr dv l

hdtCalcPoint :: TresholdRange -> Deviation -> LengthElems -> Point
hdtCalcPoint  r s l = Point (n, rs)
  where log2 = logBase 2
        rs =  log2 $ r / s
        n =  (log2 . fromIntegral) l
