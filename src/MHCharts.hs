module MHCharts where 
    
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo 
import qualified MHDataTypes as D

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,x) | x <- xs ]

pts :: Double -> [Double] -> [(Double,Double)]
pts f xs = [ (x,f) | x <- xs]


testPoints :: Fractional a => [(a,a)]
testPoints = [(0,2), (1,0), (2,0), (3,2), (4,6)] 

order3dpA e x = 2 * ( - (x ^ 2)) * (e x)      
order3dpB e x = 2 * (-x) * (e x)           
order3dpC e x = -2 *    (e x)          
order3Y e x y = 2 * y * (e x)          

-- leastSquaresThird ::  [(Double, Double)] -> (Double,Double,Double,Double)
leastSquaresThird ps = (s1,s2,s3)
  where ea = (\x -> -x ^ 2) 
        aa = order3dpA ea
        ab = order3dpB ea
        ac = order3dpC ea
        ay = order3Y ea 
        eb = (\x -> -x)
        ba = order3dpA eb
        bb = order3dpB eb
        bc = order3dpC eb
        by = order3Y eb
        ec = (\x -> -1)
        ca = order3dpA ec
        cb = order3dpB ec
        cc = order3dpC ec
        cy = order3Y ec       
        s1 = foldr (\(x,y) (i,j,k,v) -> (i + aa x, j + ab x, k + ac x, v + ay x y)) (0,0,0,0) ps
        s2 = foldr (\(x,y) (i,j,k,v) -> (i + ba x, j + bb x, k + bc x, v + by x y)) (0,0,0,0) ps
        s3 = foldr (\(x,y) (i,j,k,v) -> (i + ca x, j + cb x, k + cc x, v + cy x y)) (0,0,0,0) ps

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


-- graphicPoints :: IO ()
graphicPoints plotPoints normal mmq  = toFile def "graphics/defalult+mmq.png" $ do
    layout_title .= "Default + MMQ"
    plot normal
    plot mmq
    plot plotPoints