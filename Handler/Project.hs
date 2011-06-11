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
import Fields.Users
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
  (prid, project, owners, ownProfiles) <- getProjectAndOwnersOr404 name
  mu <- maybeAuthId
  let canEdit = maybe False (`elem` owners) mu
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
                      setMessage . toHtml $ join ", " ts
                      redirect RedirectTemporary ProjectCreateR
    _ -> redirect RedirectTemporary ProjectCreateR

getUserPermissionsR :: Text -> Handler RepHtml
getUserPermissionsR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  allps <- runDB (filter ((/= uId) . profileUser) . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
  let names = map (fromString . Text.unpack . profileFullName) allps
      auths = map (Just . (`elem` owners) . profileUser) allps
      allUids = map profileUser allps
  ((res, form), enctype) <- runFormPost $ renderTable $ userAccessField allUids names auths
  defaultLayout $ do
    setTitle "Add Users"
    addWidget $(widgetFile "getUsers")

postUserPermissionsR :: Text -> Handler ()
postUserPermissionsR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  allps <- runDB (filter ((/= uId) . profileUser) . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
  let names = map (fromString . Text.unpack . profileFullName) allps
      auths = map (Just . (`elem` owners) . profileUser) allps
      allUids = map profileUser allps
  ((res, form), enctype) <- runFormPost $ renderTable $ userAccessField allUids names auths
  case res of
    FormSuccess auths -> do
             runDB $ mapM (\(uid,t) -> if t 
                                       then insert (ProjectUser prid uid) >> return ()
                                       else deleteBy (UniqueProjectUser uid prid)
                          ) auths
             redirect RedirectTemporary (ProjectR name)
    FormFailure ts -> do
             setMessage . toHtml $ join ", " ts
             redirect RedirectTemporary (ProjectR name)
    _ -> redirect RedirectTemporary (ProjectR name)

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

getProjectAndOwnersOr404 prjName = do
    (prid, project) <- runDB $ getBy404 $ UniqueProject prjName
    os <- runDB $ (map snd) <$> selectList [ProjectUserProjectEq prid] [] 0 0
    let owners = map projectUserUser os
    ownProfiles <- runDB $ mapM (((snd <$>)<$>) . getBy . UniqueProfile) owners
    return (prid, project, owners, ownProfiles)

getOwners prid = runDB (map (projectUserUser . snd) <$> selectList [ProjectUserProjectEq prid] [] 0 0)

profileFullName p = profileFirstName p <++> " " <++> profileLastName p
