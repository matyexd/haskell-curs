{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BanquetsInterface where

import Text.Read (readMaybe)
import MenuInterface
import GHC.Generics

data SelectedDishes = SelectedDishes
  { id :: Int
  , count :: Int
  } deriving (Show, Generic)

addNewDishes :: [SelectedDishes] -> IO [SelectedDishes]
addNewDishes selectedDishes = do
  putStrLn "Введите id блюда (или '0' для завершения):"
  dishIdStr <- getLine

  -- Попробовать преобразовать в число
  let maybeDishId = readMaybe dishIdStr :: Maybe Int

  case maybeDishId of
    Just dishId | dishId == 0 -> do
      putStrLn "Ввод завершен"
      return selectedDishes
    Just dishId -> do
      putStrLn "Введите количество:"
      dishCountStr <- getLine
      let maybeDishCount = readMaybe dishCountStr :: Maybe Int

      case maybeDishCount of
        Just dishCount -> do
          let newDish = SelectedDishes { BanquetsInterface.id = dishId, BanquetsInterface.count = dishCount }
          let updatedDishes = selectedDishes ++ [newDish]

          putStrLn $ "Блюдо добавлено: " ++ show newDish
          addNewDishes updatedDishes
        Nothing -> do
          putStrLn "Некорректное количество. Повторите ввод."
          addNewDishes selectedDishes

    Nothing -> do
      -- Если преобразование в число не удалось, обрабатываем ввод как текст
      putStrLn "Введите текстовое значение (например, 'menu'):"
      let dishIdText = dishIdStr -- Просто используем введенное значение как текст
      putStrLn $ "Вы ввели текст: " ++ dishIdText
      -- Здесь можно добавить дополнительные действия для текстового ввода

      if (dishIdText == "menu") then do
        optionsViewingDishes(True)
        addNewDishes selectedDishes
      else
        addNewDishes selectedDishes



optionsBookBanquet :: IO ()
optionsBookBanquet = do
  putStrLn "Бронирование банкета. Введите блюда и их количество:"

  -- Вызываем рекурсивную функцию для добавления блюд
  selectedDishes <- addNewDishes []

  -- Выводим результат
  putStrLn "Выбранные блюда для банкета:"
  mapM_ (\dish -> putStrLn $ show dish) selectedDishes
