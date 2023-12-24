{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BanquetModule where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Time.Format
import Data.Time.Calendar

data BanquetItem = BanquetItem
  {
    id :: Int
  , date :: Day
  , price :: Double
  } deriving (Show, Generic)

instance FromJSON BanquetItem
instance ToJSON BanquetItem


getBanquets :: IO [BanquetItem]
getBanquets = do
  jsonContent <- B.readFile "app/db/banquets.json"

  case decode (BSLU.fromString (BSLU.toString jsonContent)) of
    Nothing -> do
      putStrLn "Error parsing JSON"
      return []
    Just ingredientsItems -> do
      return ingredientsItems

-- Функция для добавления нового элемента в список и записи в файл
addBanquet :: BanquetItem -> IO (Int)
addBanquet newItem = do
  -- Загрузим текущий список
  existingBanquets <- getBanquets

  -- Получим максимальный id из текущего списка (или 0, если список пуст)
  let maxId = fromMaybe 0 (maximumId existingBanquets)

  -- Создадим новый элемент с увеличенным id
  let newItemWithId = newItem { BanquetModule.id = maxId + 1 }

  -- Добавим новый элемент к существующему списку
  let updatedBanquets = existingBanquets ++ [newItemWithId]

  -- Запишем обновленный список в файл
  writeBanquets updatedBanquets

  return (maxId + 1)

isDateAvailable :: Day -> [BanquetItem] -> Bool
isDateAvailable newDate existingBanquets =
  notElem newDate (map date existingBanquets)

-- Функция для записи списка банкетов в файл
writeBanquets :: [BanquetItem] -> IO ()
writeBanquets banquets = do
  let jsonContent = encode banquets
  B.writeFile "app/db/banquets.json" jsonContent

-- Вспомогательная функция для получения максимального id из списка
maximumId :: [BanquetItem] -> Maybe Int
maximumId banquets = maximumMay (map BanquetModule.id banquets)

-- Вспомогательная функция, аналог maximum, но возвращает Maybe
maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just (maximum xs)

