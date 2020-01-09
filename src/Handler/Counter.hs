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
  sendStatusJSON noContent204 (object [])
  