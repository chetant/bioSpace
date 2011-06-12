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
  isAdmin <- maybe (return False) checkAdmin mu
  let canEdit = maybe isAdmin (`elem` owners) mu
  defaultLayout $ do
    setTitle . toHtml $ "Genspace - Project - " <++> (projectName project)
    addWidget $(widgetFile "project")

getProjectCreateR :: Handler RepHtml
getProjectCreateR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ projectFormlet Nothing
  defaultLayout $ do
    setTitle "Create New Project"
    let objName :: Text
        objName = "Project"
        actionName :: Text
        actionName = "Create"
    addWidget $(widgetFile "createEdit")

postProjectCreateR :: Handler ()
postProjectCreateR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ projectFormlet Nothing
  case res of
    FormSuccess project -> do
                       runDB $ do
                             prid <- insert project
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
getProjectEditR :: Text -> Handler RepHtml
getProjectEditR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ projectFormlet $ Just project
  defaultLayout $ do
    setTitle "Edit Project"
    let objName :: Text
        objName = "Project"
        actionName :: Text
        actionName = "Update"
    addWidget $(widgetFile "createEdit")

postProjectEditR :: Text -> Handler ()
postProjectEditR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ projectFormlet $ Just project
  case res of
    FormSuccess project -> do
                       runDB $ replace prid project
                       redirect RedirectTemporary (ProjectR (projectName project))
    FormFailure ts -> do
                      setMessage . toHtml $ join ", " ts
                      redirect RedirectTemporary (ProjectEditR name)
    _ -> redirect RedirectTemporary (ProjectEditR name)

getProjectDeleteR :: Text -> Handler RepHtml
getProjectDeleteR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Are You Sure?" (Just False)
  defaultLayout $ do
    setTitle "Project Delete Confirmation"
    addWidget $ [hamlet|
<h1> Deletion Confirmation - Project #{name}
<form enctype="#{enctype}" method=POST>
    ^{form}
    <input type="submit" value="Submit">
|]

postProjectDeleteR :: Text -> Handler RepHtml
postProjectDeleteR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Confirmed" (Just False)
  case res of
    FormSuccess True -> runDB $ do
                            -- delete all references to prid
                            userPerms <- map fst <$> selectList [ProjectUserProjectEq prid] [] 0 0
                            mapM_ delete userPerms
                            -- delete project itself
                            delete prid
    FormFailure ts -> setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
    _ -> return ()
  redirect RedirectTemporary (ProjectR name)

---------------------------
----- Helper functions ----
---------------------------

projectFormlet project = renderTable $ Project
                         <$> areq textField "Name" (projectName <$> project)
                         <*> areq textField "Description" (projectDescription <$> project)

getProjectAndOwnersOr404 prjName = do
    (prid, project) <- runDB $ getBy404 $ UniqueProject prjName
    os <- runDB $ (map snd) <$> selectList [ProjectUserProjectEq prid] [] 0 0
    let owners = map projectUserUser os
    ownProfiles <- runDB $ mapM (((snd <$>)<$>) . getBy . UniqueProfile) owners
    return (prid, project, owners, ownProfiles)

getOwners prid = runDB (map (projectUserUser . snd) <$> selectList [ProjectUserProjectEq prid] [] 0 0)
