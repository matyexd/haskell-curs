{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BanquetsInterface where

import Text.Read (readMaybe)
import MenuInterface
import GHC.Generics

import DishModule
import BanquetModule
import BanquetsDishesModule

import Data.Time.Format
import Data.Time.Calendar

data SelectedDishes = SelectedDishes
  { id :: Int
  , count :: Int
  } deriving (Show, Generic)

-- Функция для получения цены блюда по его id
getDishPrice :: Int -> [MenuItem] -> Maybe Double
getDishPrice targetId menuItems = do
  dish <- getDish targetId menuItems
  return (DishModule.price dish)

-- Функция для подсчета общей цены банкета
calculateBanquetPrice :: [SelectedDishes] -> [MenuItem] -> Maybe Double
calculateBanquetPrice selectedDishes menuItems = do
  prices <- traverse (\dish -> (*) <$> getDishPrice (BanquetsInterface.id dish) menuItems <*> pure (fromIntegral (BanquetsInterface.count dish))) selectedDishes
  return (sum prices)

-- Функция для ввода даты
enterDate :: IO Day
enterDate = do
  putStrLn "Введите дату в формате 'гггг-мм-дд' (например, '2023-10-20'):"
  dateStr <- getLine

  let maybeDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr :: Maybe Day
  case maybeDate of
    Just date -> do
      banquets <- getBanquets
      if (isDateAvailable date banquets) then do
        putStrLn "Дата введена успешно.\n"
        return date
      else do
        putStrLn "Этот день занят другим банкетом, укажите другую дату"
        enterDate
    Nothing -> do
      putStrLn "Некорректный формат даты. Повторите ввод."
      enterDate


addNewDishes :: [SelectedDishes] -> IO [SelectedDishes]
addNewDishes selectedDishes = do
  putStrLn "Введите id блюда ('0' для завершения, 'menu' - для просмотра меню):"
  dishIdStr <- getLine

  -- Попробовать преобразовать в число
  let maybeDishId = readMaybe dishIdStr :: Maybe Int

  case maybeDishId of
    Just dishId | dishId == 0 -> do
      putStrLn "\nВвод завершен\n"
      return selectedDishes
    Just dishId -> do

      menuItems <- getDishes
      let maybeDish = getDish dishId menuItems

      case maybeDish of
        Just dish -> do
          putStrLn $ "\nНайдено блюдо: " ++ DishModule.name dish
          putStrLn "Введите количество:"
          dishCountStr <- getLine
          let maybeDishCount = readMaybe dishCountStr :: Maybe Int

          case maybeDishCount of
            Just dishCount -> do
              let newDish = SelectedDishes { BanquetsInterface.id = dishId, BanquetsInterface.count = dishCount }
              let updatedDishes = selectedDishes ++ [newDish]

              putStrLn $ "\nБлюдо добавлено: " ++ DishModule.name dish
              addNewDishes updatedDishes
            Nothing -> do
              putStrLn "Некорректное количество. Повторите ввод."
              addNewDishes selectedDishes
        Nothing -> do
          putStrLn $ "\nБлюдо с id " ++ show dishId ++ " не найдено.\n"
          addNewDishes selectedDishes


    Nothing -> do
      -- Если преобразование в число не удалось, обрабатываем ввод как текст
      let dishIdText = dishIdStr

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

  menuItems <- getDishes

  -- Выводим результат
  putStrLn "Выбранные блюда для банкета:"
  mapM_ (\dish -> putStrLn $ maybe ("Неизвестная категория") DishModule.name (getDish (BanquetsInterface.id dish) menuItems) ++ " - " ++ show (BanquetsInterface.count dish) ++ " штук") selectedDishes

  bookingDate <- enterDate
  putStrLn $ "Дата бронирования: " ++ show bookingDate

  case calculateBanquetPrice selectedDishes menuItems of
    Just totalPrice -> do
      putStrLn $ "Общая стоимость банкета: " ++ show totalPrice
      putStrLn("\n")
      newBanquetId <- addBanquet (BanquetItem 0 bookingDate totalPrice)
      mapM_ (\dish -> addBanquetsDishesItem (BanquetsInterface.id dish) newBanquetId (BanquetsInterface.count dish)) selectedDishes
    Nothing -> putStrLn "Не удалось посчитать общую стоимость банкета."
