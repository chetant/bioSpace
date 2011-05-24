{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Profile where

import Control.Applicative((<$>))
import BioSpace

getPeopleR :: Handler RepHtml
getPeopleR = do
  -- getBy $ UniqueProfile (fst mu)
  people <- runDB $ selectList [] [] 0 0
  defaultLayout $ do
               setTitle "Genspace - People"
               addWidget $(widgetFile "people")

-- getPersonR :: Handler RepHtml
getPersonR profileId = do
  person <- runDB $ get404 profileId
  defaultLayout $ do
    setTitle "Genspace - People"
    addWidget $(widgetFile "person")

-- getPersonByNameR fName lName = do
--   (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
--   defaultLayout $ do
--     setTitle "Genspace - People"
--     addWidget $(widgetFile "person")
