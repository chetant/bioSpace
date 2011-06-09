{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Project where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Yesod.Auth
import Yesod.Auth.HashDB(UserId, addUser, changePasswd)
import Yesod.Form.Nic

import Fields.ImageUpload
import Handler.Commons
import BioSpace

getProjectsR :: Handler RepHtml
getProjectsR = do
  mu <- maybeAuthId
  projects <- map snd <$> (runDB $ selectList [] [] 0 0)
  isAdmin <- maybe (return False) checkAdmin mu
  defaultLayout $ do
               setTitle "Genspace - Projects"
               addWidget $(widgetFile "projects")

getProjectR :: Text -> Handler RepHtml
getProjectR name = do
  (prid, project) <- runDB $ getBy404 $ UniqueProject name
  mu <- maybeAuthId
  authUids <- runDB $ (projectUserUser . snd <$>) <$> selectList [ProjectUserProjectEq prid] [] 0 0
  let canEdit = maybe False (`elem` authUids) mu
  defaultLayout $ do
    setTitle . toHtml $ "Genspace - Project - " <++> (projectName project)
    addWidget $(widgetFile "project")

getProjectCreateR :: Handler RepHtml
getProjectCreateR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ projectFormlet Nothing
  defaultLayout $ do
    setTitle "Create New Project"
    addWidget $(widgetFile "createUser")

postProjectCreateR :: Handler ()
postProjectCreateR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ projectFormlet Nothing
  case res of
    FormSuccess project -> do
                       runDB $ do
                             prid <- insert $ Project (projectName project) 
                                     (projectDescription project)
                             insert $ ProjectUser prid uId
                       redirect RedirectTemporary (ProjectR (projectName project))
    FormFailure ts -> do
                      setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
                      redirect RedirectTemporary ProjectCreateR
    _ -> redirect RedirectTemporary ProjectCreateR

-- /edit/project/#Text ProjectEditR GET POST

-- getProjectEditR :: Text -> Handler RepHtml
-- getProjectEditR fName lName = do
--   uId <- requireAuthId
--   (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
--   canEdit <- checkAuth pId uId
--   unless canEdit $ permissionDenied "Not Authorized"
--   isAdmin <- checkAdmin uId
--   ((res, form), enctype) <- runFormPost $ profileFormlet uId isAdmin (Just person)
--   defaultLayout $ do
--     setTitle $ toHtml ("Edit Profile - " <++> fName <++> " " <++> lName)
--     addWidget $(widgetFile "editProfile")

-- postProjectEditR :: Text -> Text -> Handler RepHtml
-- postProjectEditR fName lName = do
--   uId <- requireAuthId
--   (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
--   canEdit <- checkAuth pId uId
--   unless canEdit $ permissionDenied "Not Authorized"
--   isAdmin <- checkAdmin uId
--   ((res, form), enctype) <- runFormPost $ profileFormlet (profileUser person) isAdmin (Just person)
--   case res of
--     FormSuccess profile -> do
--              -- Only Admin can make profile.isAdmin = True
--              liftIO $ putStrLn $ "Got Project:" ++ show profile
--              when ((not isAdmin) && profileIsAdmin profile) $ permissionDenied "Not Authorized"
--              liftIO $ putStrLn "REPLACE!"
--              runDB $ replace pId profile
--              redirect RedirectTemporary (ProjectR (profileFirstName profile) (profileLastName profile))
--     FormFailure ts -> do
--              setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
--              redirect RedirectTemporary (ProjectEditR fName lName)
--     _ -> do setMessage . toHtml . Text.pack $ "Form missing!"
--             redirect RedirectTemporary (ProjectEditR fName lName)

-- getPersonDeleteR :: Text -> Text -> Handler RepHtml
-- getPersonDeleteR fName lName = do
--   uId <- requireAuthId
--   (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
--   isAdmin <- checkAdmin uId
--   unless isAdmin $ permissionDenied "Not Authorized"
--   when ((profileUser person) == uId) $ permissionDenied "Cannot delete self"
--   ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Are You Sure?" (Just False)
--   defaultLayout $ do
--     setTitle "User Delete Confirmation"
--     addWidget $ [hamlet|
-- <h1> Deletion Confirmation - #{fName} #{lName}
-- <form enctype="#{enctype}" method=POST>
--     ^{form}
--     <input type="submit" value="Submit">
-- |]

-- postPersonDeleteR :: Text -> Text -> Handler RepHtml
-- postPersonDeleteR fName lName = do
--   uId <- requireAuthId
--   (pId, person) <- runDB $ getBy404 $ ProfileFullName fName lName
--   isAdmin <- checkAdmin uId
--   unless isAdmin $ permissionDenied "Not Authorized"
--   ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Confirmed" (Just False)
--   case res of
--     FormSuccess True -> runDB $ delete pId >> delete (profileUser person)
--     FormFailure ts -> setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
--     _ -> return ()
--   redirect RedirectTemporary PeopleR

-- getAdminCreateR :: Handler RepHtml
-- getAdminCreateR = do
--   -- We can only run this if no admin created yet
--   numAdmins <- runDB $ length <$> selectList [ProfileIsAdminEq True] [] 0 0
--   unless (numAdmins == 0) $ permissionDenied "Admin already created"
--   ((res, form), enctype) <- runFormPost $ userFormlet Nothing
--   defaultLayout $ do
--     setTitle "Create Admin"
--     addWidget $(widgetFile "createUser")

-- postAdminCreateR :: Handler ()
-- postAdminCreateR = do
--   numAdmins <- runDB $ length <$> selectList [ProfileIsAdminEq True] [] 0 0
--   unless (numAdmins == 0) $ permissionDenied "Admin already created"
--   ((res, form), enctype) <- runFormPost $ userFormlet Nothing
--   case res of
--     FormSuccess lclUser -> do
--                       runDB $ do
--                              uid <- addUser (username lclUser) (passwd lclUser)
--                              insert $ Profile uid True False Nothing Nothing 
--                                    "Admin" "Administrator" "overseer of the site!" Nothing Nothing
--                       redirect RedirectTemporary RootR
--     FormFailure ts -> do
--                      setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
--                      redirect RedirectTemporary AdminCreateR
--     _ -> redirect RedirectTemporary AdminCreateR


---------------------------
----- Helper functions ----
---------------------------

-- toMaybe :: (IsString a, Eq a) => a -> Maybe a
-- toMaybe a
--     | a == fromString "" = Nothing
--     | otherwise = Just a

projectFormlet project = renderDivs $ Project
                         <$> areq textField "Name" (projectName <$> project)
                         <*> areq textField "Description" (projectDescription <$> project)

-- profileFormlet uid True p = renderTable $ Profile uid
--                    <$> areq boolField "Admin Rights" (profileIsAdmin <$> p)
--                    <*> areq boolField "Profile Visible" (profileIsVisible <$> p)
--                    -- <*> (aopt textField "Img1" (profileIconImage <$> p))
--                    -- <*> (aopt textField "Img2" (profileFullImage <$> p))
--                    <*> imageFieldOpt "Icon Image" (profileIconImage <$> p)
--                    <*> imageFieldOpt "Full Image" (profileFullImage <$> p)
--                    <*> areq textField "First Name" (profileFirstName <$> p)
--                    <*> areq textField "Last Name" (profileLastName <$> p)
--                    <*> (unTextarea <$> areq textareaField "Description" (Textarea . profileAbout <$> p))
--                    <*> aopt emailField "Email" (profileEmail <$> p)
--                    <*> aopt urlField "Website" (profileWebsite <$> p)

-- profileFormlet uid False p = renderTable $ Profile uid (maybe False id $ profileIsAdmin <$> p) (maybe True id $ profileIsVisible <$> p) Nothing Nothing
--                    <$> areq textField "First Name" (profileFirstName <$> p)
--                    <*> areq textField "Last Name" (profileLastName <$> p)
--                    <*> (unTextarea <$> areq textareaField "Description" (Textarea . profileAbout <$> p))
--                    <*> aopt emailField "Email" (profileEmail <$> p)
--                    <*> aopt urlField "Website" (profileWebsite <$> p)
