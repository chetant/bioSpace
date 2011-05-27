{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Profile where

import Control.Applicative((<$>))
import Data.Text(Text)
import qualified Data.Text as Text
import BioSpace

getPeopleR :: Handler RepHtml
getPeopleR = do
  -- getBy $ UniqueProfile (fst mu)
  mu <- maybeAuth
  people <- runDB $ selectList [] [] 0 0
  isAdmin <- maybe (return False) checkAdmin mu
  defaultLayout $ do
               setTitle "Genspace - People"
               addWidget $(widgetFile "people")

getPersonR fName lName = do
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  mu <- maybeAuth
  canEdit <- maybe (return False) (checkAuth pId) mu
  defaultLayout $ do
    setTitle "Genspace - People"
    addWidget $(widgetFile "person")

mangleEmail :: Text -> Text
mangleEmail emailAddx = user `Text.append` " < at > " `Text.append` domain
    where (user, domain) = Text.breakOn "@" emailAddx

checkAdmin (uid,_) = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ profileIsAdmin user

checkAuth pId (uid,_) = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ (uId == pId) || (profileIsAdmin user)

