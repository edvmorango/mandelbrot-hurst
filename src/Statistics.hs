module Statistics where





type MinElem = Double
type MaxElem = Double
type AccElems = Double
type LengthElems = Integer
type Elems = [Double]

data Elements = Elements Elems LengthElems AccElems MaxElem MinElem

type Average = Double
type AccAverage = Double
type TresholdRange = Double
type Deviation = Double

data HurstDataType =
   HDT Elements Average AccAverage TresholdRange Deviation
