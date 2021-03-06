{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Profile where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Lazy(toStrict)
import Text.Hamlet(renderHtmlText, preEscapedText)
import Data.String
import Yesod.Auth
import Yesod.Auth.HashDB(UserId, addUser, changePasswd)
import Yesod.Form.Nic

import Fields.ImageUpload
import Handler.Commons
import BioSpace

instance GridResource Profile where
    getImageUrl =  fromMaybe "/static/img/NoIcon.png" . (("/static/uploads/" <++>) <$>) . profileIconImage
    getImageWidth = const "150px"
    getImageHeight = const "150px"
    getTitle = profileFullName
    getUrl p = "/person/" <++> profileFirstName p <++> "/" <++> profileLastName p

getPeopleR :: Handler RepHtml
getPeopleR = getPeopleFilterR "all"

getPeopleFilterR :: Text -> Handler RepHtml
getPeopleFilterR pType = do
  mu <- maybeAuthId
  let pFilter = makeProfileFilter pType
  people <- (filter pFilter . map snd) <$> (runDB $ selectList [] [] 0 0)
  let visPeople = filter profileIsVisible people
      numPeople = length visPeople
      numCols = 4 -- ((min 4) . round . sqrt . fromIntegral) numPeople
      grid = gridWidget numCols visPeople
  isAdmin <- maybe (return False) checkAdmin mu
  defaultLayout $ do
               setTitle "Genspace - People"
               addWidget $(widgetFile "people")

getPersonR :: Text -> Text -> Handler RepHtml
getPersonR fName lName = do
  mret <- runDB $ getBy $ ProfileFullName fName lName
  case mret of
    Just (pId, person) -> do
                           mu <- maybeAuthId
                           canEdit <- maybe (return False) (checkAuth pId) mu
                           defaultLayout $ do
                             setTitle "Genspace - People"
                             let description = addHtml (preEscapedText . profileAbout $ person)
                             addWidget $(widgetFile "person")
    _ -> notFound

getPersonCreateR :: Handler RepHtml
getPersonCreateR = do
  uId <- requireAuthId
  isAdmin <- checkAdmin uId
  unless isAdmin $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ userFormlet Nothing
  defaultLayout $ do
    setTitle "Create New User"
    let objName :: Text
        objName = "Create User"
        actionName :: Text
        actionName = "Create"
    addWidget $(widgetFile "createEdit")

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
                             insert $ Profile uid False False Student Nothing Nothing 
                                   "New" "User" "Something about the user" Nothing Nothing
                      redirect RedirectTemporary (PersonEditR "New" "User")
    FormFailure ts -> do
                      setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
                      redirect RedirectTemporary PersonCreateR
    _ -> redirect RedirectTemporary PersonCreateR

getPersonEditR :: Text -> Text -> Handler RepHtml
getPersonEditR fName lName = do
  uId <- requireAuthId
  mret <- runDB $ getBy $ ProfileFullName fName lName
  case mret of
    Just (pId, person) -> do
              canEdit <- checkAuth pId uId
              unless canEdit $ permissionDenied "Not Authorized"
              isAdmin <- checkAdmin uId
              ((res, form), enctype) <- runFormPost $ profileFormlet uId isAdmin (Just person)
              defaultLayout $ do
                               setTitle $ toHtml ("Edit Profile - " <++> fName <++> " " <++> lName)
                               let objName :: Text
                                   objName = "Edit Profile" <++> " - " <++> profileFullName person
                                   actionName :: Text
                                   actionName = "Update"
                               addWidget $(widgetFile "createEdit")
    _ -> notFound

postPersonEditR :: Text -> Text -> Handler RepHtml
postPersonEditR fName lName = do
  uId <- requireAuthId
  mret <- runDB $ getBy $ ProfileFullName fName lName
  case mret of
    Just (pId, person) -> do
              canEdit <- checkAuth pId uId
              unless canEdit $ permissionDenied "Not Authorized"
              isAdmin <- checkAdmin uId
              ((res, form), enctype) <- runFormPost $ profileFormlet (profileUser person) isAdmin (Just person)
              case res of
                FormSuccess profile -> do
                                -- Only Admin can make profile.isAdmin = True
                                when ((not isAdmin) && profileIsAdmin profile) $ permissionDenied "Not Authorized"
                                runDB $ replace pId profile
                                redirect RedirectTemporary (PersonR (profileFirstName profile) (profileLastName profile))
                FormFailure ts -> do
                                setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
                                redirect RedirectTemporary (PersonEditR fName lName)
                _ -> do setMessage . toHtml . Text.pack $ "Form missing!"
                        redirect RedirectTemporary (PersonEditR fName lName)
    _ -> notFound

getPersonDeleteR :: Text -> Text -> Handler RepHtml
getPersonDeleteR fName lName = do
  uId <- requireAuthId
  mret <- runDB $ getBy $ ProfileFullName fName lName
  case mret of
    Just (pId, person) -> do
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
    _ -> notFound

postPersonDeleteR :: Text -> Text -> Handler RepHtml
postPersonDeleteR fName lName = do
  uId <- requireAuthId
  mret <- runDB $ getBy $ ProfileFullName fName lName
  case mret of
    Just (pId, person) -> do
              isAdmin <- checkAdmin uId
              unless isAdmin $ permissionDenied "Not Authorized"
              ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Confirmed" (Just False)
              case res of
                FormSuccess True -> runDB $ delete pId >> delete (profileUser person)
                FormFailure ts -> setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
                _ -> return ()
              redirect RedirectTemporary PeopleR
    _ -> notFound
  

getAdminCreateR :: Handler RepHtml
getAdminCreateR = do
  -- We can only run this if no admin created yet
  numAdmins <- runDB $ length <$> selectList [ProfileIsAdminEq True] [] 0 0
  unless (numAdmins == 0) $ permissionDenied "Admin already created"
  ((res, form), enctype) <- runFormPost $ userFormlet Nothing
  defaultLayout $ do
    setTitle "Create Admin"
    let objName :: Text
        objName = "Create Admin"
        actionName :: Text
        actionName = "Create"
    addWidget $(widgetFile "createEdit")

postAdminCreateR :: Handler ()
postAdminCreateR = do
  numAdmins <- runDB $ length <$> selectList [ProfileIsAdminEq True] [] 0 0
  unless (numAdmins == 0) $ permissionDenied "Admin already created"
  ((res, form), enctype) <- runFormPost $ userFormlet Nothing
  case res of
    FormSuccess lclUser -> do
                      runDB $ do
                             uid <- addUser (username lclUser) (passwd lclUser)
                             insert $ Profile uid True False Student Nothing Nothing 
                                   "Admin" "Administrator" "overseer of the site!" Nothing Nothing
                      redirect RedirectTemporary RootR
    FormFailure ts -> do
                     setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
                     redirect RedirectTemporary AdminCreateR
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
    FormFailure ts -> setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
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

userFormlet user = renderTable $ LclUser
                   <$> areq textField "Username" (username <$> user)
                   <*> areq passwordField "Password" (passwd <$> user)

profileFormlet uid True p = renderDivs $ Profile uid
                   <$> areq boolField "Admin Rights" (profileIsAdmin <$> p)
                   <*> areq boolField "Profile Visible" (profileIsVisible <$> p)
                   <*> areq (selectField userTypes) "Type" (profileType <$> p)
                   <*> imageFieldOpt "Icon Image" (profileIconImage <$> p)
                   <*> imageFieldOpt "Full Image" (profileFullImage <$> p)
                   <*> areq textField "First Name" (profileFirstName <$> p)
                   <*> areq textField "Last Name" (profileLastName <$> p)
                   <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic descFS (preEscapedText . profileAbout <$> p)))
                   <*> aopt emailField "Email" (profileEmail <$> p)
                   <*> aopt urlField "Website" (profileWebsite <$> p)
    where userTypes = [("Member", Member), ("Co-Founder",CoFounder)
                      ,("Collaborator", Collaborator), ("Student",Student)
                      ,("Presenter", Presenter)]
          descFS :: FieldSettings Text
          descFS = FieldSettings "Description" Nothing (Just "description") (Just "description")

profileFormlet uid False p = renderTable $ Profile uid (fromMaybe False $ profileIsAdmin <$> p) 
                                                       (fromMaybe True  $ profileIsVisible <$> p)
                                                       (fromMaybe Student $ profileType <$> p)
                   <$> imageFieldOpt "Icon Image" (profileIconImage <$> p)
                   <*> imageFieldOpt "Full Image" (profileFullImage <$> p)
                   <*> areq textField "First Name" (profileFirstName <$> p)
                   <*> areq textField "Last Name" (profileLastName <$> p)
                   <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic "Description" (preEscapedText . profileAbout <$> p)))
                   <*> aopt emailField "Email" (profileEmail <$> p)
                   <*> aopt urlField "Website" (profileWebsite <$> p)

makeProfileFilter :: Text -> (Profile -> Bool)
makeProfileFilter "cofounders"   = ( == CoFounder) . profileType
makeProfileFilter "members"       = ( == Member) . profileType
makeProfileFilter "collaborators" = (== Collaborator) . profileType
makeProfileFilter "students"      = (== Student) . profileType
makeProfileFilter "presenters"    = (== Presenter) . profileType
makeProfileFilter _ = const True
