module MHDataTypes  where

type MinElem = Double
type MaxElem = Double

type AccElems = Double
type LengthElems = Integer
type Elems = [Double]

type Average = Double
type AccAverage = Double
type TresholdRange = Double
type Deviation = Double

data Extremes = Ex MinElem MaxElem | Inv deriving (Eq, Ord , Show)

data Elements = Elements Elems LengthElems AccElems Extremes
  deriving (Eq, Show)

data HurstDataType =
   HDT Elements Average AccAverage TresholdRange Deviation deriving (Eq, Show)

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

mkElements :: Elems -> Elements
mkElements els = Elements els len acc ex
  where lambda = (\e (len, acc, ex) -> (len+1, acc+e, (mkExtremes e) `mappend` ex))
        (len, acc, ex) = foldr lambda (0, 0, Inv) els


elementsAverage :: Elements -> Average
elementsAverage (Elements _ l a _) = a / (fromIntegral l)


-- Must get max and min AccAvg not the Sum
elementsAccAverage :: Elements -> Average -> AccAverage
elementsAccAverage (Elements els _ _ _) avg = foldr (\e acc ->  (e - avg) + acc) 0 els
