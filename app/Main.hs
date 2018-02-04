{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import MHLib

import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Control.Monad (mzero)
import Data.Csv
import Data.Time
import Data.Text (Text, foldr)
import GHC.Generics (Generic)

newtype Price = Price Double deriving (Eq, Show)

type QDay = Maybe Day
data Quotation = Quotation QDay Price deriving (Show)

-- instance FromRecord Quotation where
--   parseRecord v
--     | length v == 2 = Quotation <$>  (v .! 0) --  <*> v .! 1
--     | otherwise = mzero



-- String laziness is inneficient, but many APIs uses String type
-- Where is the brigde?
getDay :: Text -> QDay
getDay v = parseTimeM True defaultTimeLocale "%d/%m/%Y" (textToString v) :: Maybe Day
  where textToString = Data.Text.foldr (\a acc -> a : acc) []


-- instance FromField (QDay) where
--   parseField s = case runParser (parseField s) of
--     (Left _) -> pure $ Nothing
--     (Right e) -> pure $ getDay e


instance FromField (Price) where
  parseField s = case runParser (parseField s :: Parser Double) of
    (Left _) -> pure $ Price 0
    (Right n) -> pure $ Price n


-- data Quotation = { date }




main :: IO ()
main = putStrLn "Here begins"
