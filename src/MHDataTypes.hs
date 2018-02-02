module MHDataTypes where

type MinElem = Double
type MaxElem = Double

type AccElems = Double
type LengthElems = Integer
type Elems = [Double]

type Average = Double
type DiffAcc = Double
type DiffAccExtremes = Extremes
type TresholdRange = Double
type Deviation = Double

data Extremes = Ex MinElem MaxElem | Inv deriving (Eq, Ord , Show)

data Elements = Elements Elems LengthElems AccElems
  deriving (Eq, Show)

data HurstDataType = HDT Elements TresholdRange Deviation deriving (Eq, Show)

instance Monoid Extremes where
  mempty = Inv
  mappend (Ex a b) (Ex a' b') = Ex (min a a') (max b b')
  mappend (Ex a b) _ = Ex a b
  mappend _  (Ex a b) = Ex a b

instance Monoid Elements where
  mempty = Elements [] 0 0
  mappend (Elements es l a) (Elements es' l' a') =
    Elements (es `mappend` es') (l + l') (a + a')

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
mkHDT es@(Elements _ _ _ ) = HDT es tr dv
  where
        tr = elementsTreRange es
        dv = elementsDeviation es
