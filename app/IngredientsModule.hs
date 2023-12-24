{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module IngredientsModule where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.List (find)

data IngredientItem = IngredientItem
  { id :: Int
  , name :: String
  } deriving (Show, Generic)

instance FromJSON IngredientItem

printIngredients :: [IngredientItem] -> IO ()
printIngredients ingredientItems =
  mapM_ (\(ingredient) -> putStrLn $ "id - " ++ show (IngredientsModule.id ingredient) ++ ", Название - " ++ IngredientsModule.name ingredient) ingredientItems

getIngredients :: IO [IngredientItem]
getIngredients = do
  jsonContent <- B.readFile "app/db/ingredients.json"

  case decode (BSLU.fromString (BSLU.toString jsonContent)) of
    Nothing -> do
      putStrLn "Error parsing JSON"
      return []
    Just ingredientsItems -> do
      return ingredientsItems

-- Функция для безопасного парсинга строки с числами, разделёнными запятой, в список чисел
parseInput :: String -> Maybe [Int]
parseInput input = sequenceA $ map parseNumber (wordsWhen (== ',') input)

parseNumber :: String -> Maybe Int
parseNumber s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'