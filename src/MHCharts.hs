module MHCharts where 
    
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo 
import Data.Time
import qualified MHDataTypes as D

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,x) | x <- xs ]

pts :: Double -> [Double] -> [(Double,Double)]
pts f xs = [ (x,f) | x <- xs]


extractPoint :: D.HurstDataType -> (Double, Double)
extractPoint (D.HDT _ _ _  (D.Point (a,b)) ) = (a,b)

extractPoints :: [D.HurstDataType] -> [[(Double, Double)]]
extractPoints xs = [ps]
  where ps = map (\x -> extractPoint x)  xs


main = toFile def "example.png" $ do
    layout_title .= "Amplitude Modulation"
    plot (line "1" ([pts 300 [7..400]] ))
    plot (line "2" ([pts 100 [7..400]] ))
    -- plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))