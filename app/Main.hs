{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import MHDataTypes
import MHLib

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

import Control.Applicative
import Control.Monad (mzero)
import Data.Csv
import Data.Time
import qualified Data.Vector as V
import qualified Data.Text  as T 
import Data.Either
import GHC.Generics (Generic)
import System.Environment


newtype Price = Price Double deriving (Eq, Show)
newtype QDay = QDay (Maybe Day) deriving (Eq, Show)

data Quotation = Quotation QDay Price deriving (Show)


instance FromRecord Quotation where
  parseRecord v
    | length v == 2 = Quotation <$>  (v .! 0)  <*> v .! 1
    | otherwise = mzero

-- String laziness is inneficient, but many APIs uses String type
-- Where is the brigde?
getDay :: T.Text -> QDay
getDay v = QDay day
  where textToString = T.foldr (\a acc -> a : acc) []
        day = parseTimeM True defaultTimeLocale "%d/%m/%Y" (textToString v) :: Maybe Day

unliftPrice :: Quotation -> Price 
unliftPrice (Quotation _ p) = p

unliftValue :: Price -> Double
unliftValue (Price p) = p

instance FromField (QDay) where
  parseField s = case runParser (parseField s) of
    (Left _) -> pure $ QDay Nothing
    (Right e) -> pure $ getDay e


instance FromField (Price) where
  parseField s = case runParser (parseField s :: Parser Double) of
    (Right n) -> pure $ Price n
    _ -> pure $ Price 0

fileToList :: BL.ByteString -> [Quotation]
fileToList f =  ( map (V.head) . rights . map (lineDecoder) . (C.split '\n')) f

lineDecoder :: C.ByteString -> Either String (V.Vector Quotation)
lineDecoder l =  decode NoHeader l


pricesCleaner :: Price -> [Quotation] -> [Quotation]
pricesCleaner _ [] = []
pricesCleaner p (h@(Quotation d hp@(Price v)) : t)
  | v == 0 = (Quotation d p) : pricesCleaner p t
  | otherwise =  h : pricesCleaner hp t
  
prepareCleaner :: [Quotation] -> [Quotation] 
prepareCleaner qs =  pricesCleaner p qs
  where p = unliftPrice $ head qs
    
clean :: BL.ByteString -> [Quotation]
clean file = (prepareCleaner . fileToList ) file

applyHurst ::  BL.ByteString ->  [HurstDataType]
applyHurst raw = makeHDTs iterations qs 
  where qs = (map (unliftValue . unliftPrice) . clean) raw
        nElements =  (fromInteger . toInteger . length) qs
        iterations = floor $ logBase (2) nElements


makeHDTs :: (Integral a) => a -> [Double] -> [HurstDataType]
makeHDTs i els = map (\x -> mkHDT (mkElements (take x els))) ranges
  where ranges = map (fromIntegral . (2^) ) [1..i] 
          
          
main :: IO ()
main = do
  args <- getArgs
  file <- BL.readFile (head args)
  putStrLn $ show (applyHurst file)
