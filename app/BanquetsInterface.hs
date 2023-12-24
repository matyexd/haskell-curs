{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BanquetsInterface where

import MenuInterface

optionsBookBanquet :: IO () -> IO ()
optionsBookBanquet back = do
    putStrLn "\nБронирование банкета. Впишите id блюда, которое хотите добавить в банкет:"
    putStrLn "Показать меню - menu"
    putStrLn "Закончить добавлять блюда - stop"
    putStrLn "Назад - back"
    choice <- getLine

    case choice of
      "menu" -> do
        optionsViewingDishes back

      "stop" -> do
        optionShowDishesByCategoryId back
      "back" -> do
        back
      _ -> do
        optionsBookBanquet back