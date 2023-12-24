module PrintDishNames where


import Data.List (isInfixOf, sortOn, find, intercalate)

import DishModule
import CategoryModule
import DishesIngredientsModule
import IngredientsModule

ingredientNamesByIds :: [Int] -> [IngredientItem] -> String
ingredientNamesByIds ids ingredientList =
  let matchingIngredients = filter (\ingredient -> IngredientsModule.id ingredient `elem` ids) ingredientList
  in intercalate ", " (map IngredientsModule.name matchingIngredients)

printDishNames :: [MenuItem] -> IO ()
printDishNames menuItems = do
  categories <- getCategories
  dishesIngredients <- getDishesIngredients
  ingredients <- getIngredients
  mapM_ (\(dish) -> putStrLn $ "id - " ++ show (DishModule.id dish) ++
    "\nНазвание - " ++ DishModule.name dish ++
    "\nЦена - " ++ show (DishModule.price dish) ++
    "\nКол-во грамм - " ++ show(DishModule.grams dish) ++
    "\nКол-во калорий - " ++ show (DishModule.calories dish) ++
    "\nКатегория - " ++ maybe "Неизвестная категория" CategoryModule.name (getCategoryById (DishModule.category_id dish) categories) ++
    "\nСостав - " ++ ingredientNamesByIds (getIngredientIds dishesIngredients dish) ingredients ++ "\n") menuItems