{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Profile where

import Control.Applicative((<$>))
import Data.Text(Text)
import qualified Data.Text as Text
import BioSpace

getPeopleR :: Handler RepHtml
getPeopleR = do
  -- getBy $ UniqueProfile (fst mu)
  people <- runDB $ selectList [] [] 0 0
  defaultLayout $ do
               setTitle "Genspace - People"
               addWidget $(widgetFile "people")

-- getPersonR :: Handler RepHtml
-- getPersonR profileId = do
--   person <- runDB $ get404 profileId
--   mu <- maybeAuth
--   defaultLayout $ do
--     setTitle "Genspace - People"
--     addWidget $(widgetFile "person")

mangleEmail :: Text -> Text
mangleEmail emailAddx = user `Text.append` " < at > " `Text.append` domain
    where (user, domain) = Text.breakOn "@" emailAddx

getPersonR fName lName = do
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  mu <- maybeAuth
  defaultLayout $ do
    setTitle "Genspace - People"
    addWidget $(widgetFile "person")
