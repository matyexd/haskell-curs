{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DishesIngredientsModule where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import Data.List (find)

import DishModule

data DishesIngredientsItem = DishesIngredientsItem
  { dishes_id :: Int
  , ingredient_id :: Int
  } deriving (Show, Generic)

instance FromJSON DishesIngredientsItem

getDishesIngredients :: IO [DishesIngredientsItem]
getDishesIngredients = do
  jsonContent <- B.readFile "app/db/dishes-ingredients.json"

  case decode (BSLU.fromString (BSLU.toString jsonContent)) of
    Nothing -> do
      putStrLn "Error parsing JSON"
      return []
    Just ingredientsItems -> do
      return ingredientsItems

getDishesDisjunction :: [DishesIngredientsItem] -> [Int] -> [MenuItem] -> [MenuItem]
getDishesDisjunction dishesIngredientsItems selectedIngredients allDishes =
  let matchingDishes = filter (\dish -> any (\ingrId -> hasIngredient dishesIngredientsItems dish ingrId) selectedIngredients) allDishes
  in matchingDishes

getDishesConjunction :: [DishesIngredientsItem] -> [Int] -> [MenuItem] -> [MenuItem]
getDishesConjunction dishesIngredientsItems selectedIngredients allDishes =
  let matchingDishes = filter (\dish -> all (\ingrId -> elem ingrId ( getIngredientIds dishesIngredientsItems dish)) selectedIngredients) allDishes
  in matchingDishes

-- Функция проверки наличия ингредиента в блюде
hasIngredient :: [DishesIngredientsItem] -> MenuItem -> Int -> Bool
hasIngredient items dish ingredientId = do
  any (\item -> DishesIngredientsModule.dishes_id item == DishModule.id dish && DishesIngredientsModule.ingredient_id item == ingredientId) items

-- Функция получения id ингредиентов из блюда
getIngredientIds :: [DishesIngredientsItem] -> MenuItem -> [Int]
getIngredientIds items dish =
  map DishesIngredientsModule.ingredient_id (filter (\item -> DishesIngredientsModule.dishes_id item == DishModule.id dish) items)
