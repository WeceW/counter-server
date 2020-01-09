{-# LANGUAGE OverloadedStrings #-}
module Handler.Counter where

import Import
import Database.Persist.Postgresql

postCounterR :: Handler Value
postCounterR = do
  counter <- requireJsonBody :: Handler Counter
  pid <- runDB $ insert counter 
  sendStatusJSON created201 (object ["id" .= fromSqlKey pid])

patchCounterAddR :: CounterId -> Handler Value
patchCounterAddR idCounter = do
  _ <- runDB $ get404 idCounter
  runDB $ update idCounter [CounterCount +=. 1]
  dt <- runDB $ selectList [CounterId ==. idCounter] []
  sendStatusJSON accepted202 (object ["content" .= (dt)])
