module MHCharts where 
    
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo 
import Data.Time
import MHDataTypes

-- data RenderQuotation =  R enderQuotation Day Point


testPoints :: Num a => [(a, a)] 
testPoints = [(1,1),(2,2),(3,3)]



signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,x) | x <- xs ]

pts :: Double -> [Double] -> [(Double,Double)]
pts f xs = [ (x,f) | x <- xs]


main = toFile def "example.png" $ do
    layout_title .= "Amplitude Modulation"
    plot (line "1" ([pts 300 [7..400]] ))
    plot (line "2" ([pts 100 [7..400]] ))
    -- plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))