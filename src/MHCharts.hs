module MHCharts where 
    
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo 
import qualified MHDataTypes as D

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,x) | x <- xs ]

pts :: Double -> [Double] -> [(Double,Double)]
pts f xs = [ (x,f) | x <- xs]


extractPoint :: D.HurstDataType -> (Double, Double)
extractPoint (D.HDT _ _ _  (D.Point (a,b)) ) = (a,b)

extractPoints :: [D.HurstDataType] -> [(Double, Double)]
extractPoints xs = map (\x -> extractPoint x)  xs
  

applyGraphicPoints :: [D.HurstDataType] -> IO ()
applyGraphicPoints hs = graphicPoints pointsGraphic normal mmq -- normal mmq
  where uPoints = extractPoints hs
        nPoints = fromIntegral (length uPoints) * 1.5 
        graphicSize = 100 -- should be dynamic
        pointsGraphic =  points "R/S points" ( (nPoints, nPoints) : uPoints)
        normal = line "Behavior" [uPoints]
        mmq = line "Normalization" [[]] 


-- graphicPoints ::  :: IO ()
graphicPoints plotPoints normal mmq  = toFile def "graphics/defalult+mmq.png" $ do
    layout_title .= "Default + MMQ"
    plot normal
    plot mmq
    plot plotPoints
    -- plot (line "1" ([pts 300 [7..400]] ))
    -- plot (line "2" ([pts 100 [7..400]] ))
    -- plot (line "am" [signal [0,(0.5)..400]])
    -- plot (points "am points" (signal [0,7..400]))