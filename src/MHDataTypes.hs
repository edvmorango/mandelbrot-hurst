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

newtype Point = Point (Double, Double) deriving (Eq, Show)
data Extremes = Ex MinElem MaxElem | Inv deriving (Eq, Ord , Show)
data Elements = Elements Elems LengthElems AccElems deriving (Eq, Show)
data HurstDataType = HDT Elements TresholdRange Deviation Point deriving (Eq)

instance Monoid Extremes where
  mempty = Inv
  mappend (Ex a b) (Ex a' b') = Ex (min a a') (max b b')
  mappend (Ex a b) _ = Ex a b
  mappend _  (Ex a b) = Ex a b

instance Monoid Elements where
  mempty = Elements [] 0 0
  mappend (Elements es l a) (Elements es' l' a') =
    Elements (es `mappend` es') (l + l') (a + a')

instance Show HurstDataType where
  show (HDT (Elements _ l _) _ _ (Point (x,y)) ) =
     "HDT (Elements " ++ (show l) ++ ") (Point ("++(show x)++", " ++ (show y)++ "))"
