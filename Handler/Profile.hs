{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Profile where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Yesod.Auth
import Yesod.Auth.HashDB(UserId, addUser, changePasswd)
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
  uId <- requireAuthId
  isAdmin <- checkAdmin uId
  unless isAdmin $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ userFormlet Nothing
  defaultLayout $ do
    setTitle "Create New User"
    addWidget $(widgetFile "createUser")

postPersonCreateR :: Handler ()
postPersonCreateR = do
  uId <- requireAuthId
  isAdmin <- checkAdmin uId
  unless isAdmin $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ userFormlet Nothing
  case res of
    FormSuccess lclUser -> do
                      runDB $ do
                             uid <- addUser (username lclUser) (passwd lclUser)
                             insert $ Profile uid False False Nothing Nothing 
                                   "New" "User" "Something about the user" Nothing Nothing
                      redirect RedirectTemporary (PersonEditR "New" "User")
    _ -> redirect RedirectTemporary PersonCreateR

getPersonEditR :: Text -> Text -> Handler RepHtml
getPersonEditR fName lName = do
  uId <- requireAuthId
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  canEdit <- checkAuth pId uId
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ profileFormlet uId False True (Just person)
  defaultLayout $ do
    setTitle $ toHtml ("Edit Profile - " <++> fName <++> " " <++> lName)
    addWidget $(widgetFile "editProfile")

postPersonEditR :: Text -> Text -> Handler RepHtml
postPersonEditR fName lName = do
  uId <- requireAuthId
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  canEdit <- checkAuth pId uId
  unless canEdit $ permissionDenied "Not Authorized"
  isAdmin <- checkAdmin uId
  ((res, form), enctype) <- runFormPost $ profileFormlet (profileUser person) False True Nothing
  case res of
    FormSuccess profile -> do
             -- Only Admin can make profile.isAdmin = True
             when ((not isAdmin) && profileIsAdmin profile) $ permissionDenied "Not Authorized"
             runDB $ replace pId profile
             redirect RedirectTemporary (PersonR (profileFirstName profile) (profileLastName profile))
    _ -> redirect RedirectTemporary (PersonEditR "New" "User")

getPersonDeleteR :: Text -> Text -> Handler RepHtml
getPersonDeleteR fName lName = do
  uId <- requireAuthId
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  isAdmin <- checkAdmin uId
  unless isAdmin $ permissionDenied "Not Authorized"
  when ((profileUser person) == uId) $ permissionDenied "Cannot delete self"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Are You Sure?" (Just False)
  defaultLayout $ do
    setTitle "User Delete Confirmation"
    addWidget $ [hamlet|
<h1> Deletion Confirmation - #{fName} #{lName}
<form enctype="#{enctype}" method=POST>
    ^{form}
    <input type="submit" value="Submit">
|]

postPersonDeleteR :: Text -> Text -> Handler RepHtml
postPersonDeleteR fName lName = do
  uId <- requireAuthId
  (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
  isAdmin <- checkAdmin uId
  unless isAdmin $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Confirmed" (Just False)
  case res of
    FormSuccess True -> runDB $ delete pId >> delete (profileUser person)
    _ -> return ()
  redirect RedirectTemporary PeopleR

getAdminCreateR :: Handler RepHtml
getAdminCreateR = do
  -- We can only run this if no admin created yet
  numAdmins <- runDB $ length <$> selectList [ProfileIsAdminEq True] [] 0 0
  unless (numAdmins == 0) $ permissionDenied "Admin already created"
  ((res, form), enctype) <- runFormPost $ userFormlet Nothing
  defaultLayout $ do
    setTitle "Create Admin"
    addWidget $(widgetFile "createUser")

postAdminCreateR :: Handler ()
postAdminCreateR = do
  numAdmins <- runDB $ length <$> selectList [ProfileIsAdminEq True] [] 0 0
  unless (numAdmins == 0) $ permissionDenied "Admin already created"
  ((res, form), enctype) <- runFormPost $ userFormlet Nothing
  case res of
    FormSuccess lclUser -> do
                      runDB $ do
                             uid <- addUser (username lclUser) (passwd lclUser)
                             insert $ Profile uid True False Nothing Nothing 
                                   "Admin" "Administrator" "overseer of the site!" Nothing Nothing
                      redirect RedirectTemporary RootR
    _ -> redirect RedirectTemporary AdminCreateR


getChangePasswdR :: Handler RepHtml
getChangePasswdR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ renderTable $ areq passwordField "New Password" Nothing
  defaultLayout $ do
    setTitle "Change Password"
    addWidget $ [hamlet|
<h1> Change Password
<form enctype="#{enctype}" method=POST>
    ^{form}
    <input type="submit" value="Submit">
|]

postChangePasswdR :: Handler ()
postChangePasswdR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ renderTable $ areq passwordField "New Password" Nothing
  case res of
    FormSuccess newPasswd -> runDB $ changePasswd uId newPasswd
    _ -> return ()
  redirect RedirectTemporary DashboardR

---------------------------
----- Helper functions ----
---------------------------

data LclUser = LclUser {
      username :: Text
    , passwd :: Text
    } deriving (Show)

toMaybe :: (IsString a, Eq a) => a -> Maybe a
toMaybe a
    | a == fromString "" = Nothing
    | otherwise = Just a

userFormlet user = renderDivs $ LclUser
                   <$> areq textField "Username" (username <$> user)
                   <*> areq passwordField "Password" (passwd <$> user)

profileFormlet uid isAdmin isVisible p = renderTable $ Profile uid isAdmin isVisible Nothing Nothing
                   <$> areq textField "First Name" (profileFirstName <$> p)
                   <*> areq textField "Last Name" (profileLastName <$> p)
                   <*> (unTextarea <$> areq textareaField "Description" (Textarea . profileAbout <$> p))
                   <*> aopt emailField "Email" (profileEmail <$> p)
                   <*> aopt urlField "Website" (profileWebsite <$> p)

mangleEmail :: Text -> Text
mangleEmail emailAddx = user <++> " < at > " <++> (Text.tail domain)
    where (user, domain) = Text.breakOn "@" emailAddx

checkAdmin uid = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ profileIsAdmin user

checkAuth pId uid = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ (uId == pId) || (profileIsAdmin user)

