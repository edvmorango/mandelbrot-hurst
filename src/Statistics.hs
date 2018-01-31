module Statistics where


type AccElems = Double
type LengthElems = Integer
type Elems = [Double]

type MinElem = Double
type MaxElem = Double

data Extremes = Ex MinElem MaxElem | Inv deriving (Eq, Show)

instance Monoid Extremes where
  mempty = Inv
  mappend (Ex a b) (Ex a' b') = Ex (min a a') (max b b')
  mappend (Ex a b) _ = Ex a b
  mappend _  (Ex a b) = Ex a b



data Elements = Elements Elems LengthElems AccElems Extremes
  deriving (Eq, Show)


type Average = Double
type AccAverage = Double
type TresholdRange = Double
type Deviation = Double

data HurstDataType =
   HDT Elements Average AccAverage TresholdRange Deviation


instance Monoid Elements where
  mempty = Elements [] 0 0 Inv
  mappend (Elements es l a e) (Elements es' l' a' e') =
    Elements (es `mappend` es') (l + l') (a + a') (e `mappend` e')
