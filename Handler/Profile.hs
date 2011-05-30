{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Profile where

import Control.Applicative((<$>),(<*>))
import Control.Monad(unless)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Yesod.Auth
import Yesod.Auth.HashDB(UserId, addUser)
import Yesod.Form.Nic
import BioSpace

(<++>) = Text.append

getPeopleR :: Handler RepHtml
getPeopleR = do
  -- getBy $ UniqueProfile (fst mu)
  mu <- maybeAuthId
  people <- map snd <$> (runDB $ selectList [] [] 0 0)
  isAdmin <- maybe (return False) checkAdmin mu
  defaultLayout $ do
               setTitle "Genspace - People"
               addWidget $(widgetFile "people")

getPersonR fName lName = do
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  mu <- maybeAuthId
  canEdit <- maybe (return False) (checkAuth pId) mu
  defaultLayout $ do
    setTitle "Genspace - People"
    addWidget $(widgetFile "person")

getPersonCreateR :: Handler RepHtml
getPersonCreateR = do
  (res, form, enctype, html) <- runFormPost $ userFormlet Nothing
  defaultLayout $ do
    setTitle "Create New User"
    addWidget $(widgetFile "createUser")

postPersonCreateR :: Handler ()
postPersonCreateR = do
  lclUser <- runFormPost' $ userFormlet Nothing
  runDB $ do
    uid <- addUser (username lclUser) (passwd lclUser)
    insert $ Profile uid False False Nothing Nothing 
             "New" "User" "Something about the user" Nothing Nothing
  redirect RedirectTemporary (PersonR "New" "User")

getEditPersonR fName lName = do
  uId <- requireAuthId
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  canEdit <- checkAuth pId uId
  unless canEdit $ permissionDenied "Not Authorized"
  (res, form, enctype, html) <- runFormPost $ profileFormlet uId False True (Just person)
  defaultLayout $ do
    setTitle $ toHtml ("Edit Profile - " <++> fName <++> " " <++> lName)
    -- TODO: why doesnt this work?
    -- addWidget $(widgetFile "editProfile")

-- Helper functions ----

data LclUser = LclUser {
      username :: Text
    , passwd :: Text
    } deriving (Show)

toMaybe :: (IsString a, Eq a) => a -> Maybe a
toMaybe a
    | a == fromString "" = Nothing
    | otherwise = Just a

userFormlet :: Formlet s m LclUser
userFormlet user = fieldsToDivs $ LclUser
                   <$> stringField "Username" (username <$> user)
                   <*> passwordField "Password" (passwd <$> user)

profileFormlet :: UserId -> Bool -> Bool -> Formlet s m Profile
profileFormlet uid isAdmin isVisible p = fieldsToDivs $ Profile uid isAdmin isVisible Nothing Nothing
                   <$> stringField "First Name" (profileFirstName <$> p)
                   <*> stringField "Last Name" (profileLastName <$> p)
                   <*> stringField "Description" (profileAbout <$> p)
                   <*> (toMaybe <$> (emailField "Email" ((maybe "" id) . profileEmail <$> p)))
                   <*> (toMaybe <$> (urlField "Website" ((maybe "" id) . profileWebsite <$> p)))

mangleEmail :: Text -> Text
mangleEmail emailAddx = user <++> " < at > " <++> (Text.tail domain)
    where (user, domain) = Text.breakOn "@" emailAddx

checkAdmin uid = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ profileIsAdmin user

checkAuth pId uid = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ (uId == pId) || (profileIsAdmin user)

