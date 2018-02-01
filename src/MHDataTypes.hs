module MHDataTypes  where

type MinElem = Double
type MaxElem = Double

type AccElems = Double
type LengthElems = Integer
type Elems = [Double]

type Average = Double
type DiffAcc = Double
type TresholdRange = Double
type Deviation = Double

data Extremes = Ex MinElem MaxElem | Inv deriving (Eq, Ord , Show)

data Elements = Elements Elems LengthElems AccElems Extremes
  deriving (Eq, Show)

data HurstDataType =
   HDT Elements Average DiffAcc TresholdRange Deviation deriving (Eq, Show)

instance Monoid Extremes where
  mempty = Inv
  mappend (Ex a b) (Ex a' b') = Ex (min a a') (max b b')
  mappend (Ex a b) _ = Ex a b
  mappend _  (Ex a b) = Ex a b

instance Monoid Elements where
  mempty = Elements [] 0 0 Inv
  mappend (Elements es l a e) (Elements es' l' a' e') =
    Elements (es `mappend` es') (l + l') (a + a') (e `mappend` e')

mkExtremes :: Double -> Extremes
mkExtremes a = Ex a a

rcExtremes :: Extremes -> Double -> Extremes
rcExtremes e v =  (mkExtremes v) `mappend` e


mkElements :: Elems -> Elements
mkElements els = Elements els len acc ex
  where lambda = (\e (len, acc, ex) -> (len+1, acc+e, rcExtremes ex e))
        (len, acc, ex) = foldr lambda (0, 0, Inv) els

elementsAverage :: Elements -> Average
elementsAverage (Elements _ l a _) = a / (fromIntegral l)

elementsDiff :: Elements -> DiffAcc
elementsDiff e@(Elements els l s _) =
  case ex of
    (Ex a b) -> b - a
    _ -> 0
  where avg = elementsAverage e
        ex = snd $ foldr (\e (acc, ex) -> let a = (e - avg) + acc  in (a, rcExtremes ex a)) (0, Inv) els

elementsDeviation :: Elements -> Deviation
elementsDeviation e@(Elements es l _ _) = sqrt (ac / (fromIntegral l) )
  where avg = elementsAverage e
        ac = foldr (\a acc -> ( (** 2) (a - avg) + acc)) 0 es
