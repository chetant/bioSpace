{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Project where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Maybe(isJust, fromJust)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Lazy(toStrict)
import Text.Hamlet(renderHtmlText, preEscapedText)
import Data.String
import Yesod.Auth
import Yesod.Auth.HashDB(UserId, addUser, changePasswd)

import Fields.ImageUpload
import Fields.Users
import Handler.Commons
import BioSpace

instance GridResource Project where
    getImageUrl = ("/static/uploads/" <++>) . projectFullImage
    getImageWidth = const "288px"
    getImageHeight = const "216px"
    getTitle = projectName
    getUrl p = "/project/" <++> projectName p

getProjectsR :: Handler RepHtml
getProjectsR = do
  mu <- maybeAuthId
  projects <- map snd <$> (runDB $ selectList [] [] 0 0)
  isAdmin <- maybe (return False) checkAdmin mu
  let grid = gridWidget 3 projects
  defaultLayout $ do
               setTitle "Genspace - Projects"
               addWidget $(widgetFile "projects")

getProjectR :: Text -> Handler RepHtml
getProjectR name = do
  (prid, project, owners, ownProfiles) <- getProjectAndOwnersOr404 name
  mu <- maybeAuthId
  isAdmin <- maybe (return False) checkAdmin mu
  let canEdit = maybe isAdmin ((|| isAdmin) . (`elem` owners)) mu
  defaultLayout $ do
    setTitle . toHtml $ "Genspace - Project - " <++> (projectName project)
    let description = addHtml (preEscapedText . projectDescription $ project)
    addWidget $(widgetFile "project")

getProjectCreateR :: Handler RepHtml
getProjectCreateR = do
  uId <- requireAuthId
  profile <- runDB $ snd <$> getBy404 (UniqueProfile uId)
  case (isEditableType . profileType) profile of
    True -> do
      ((res, form), enctype) <- runFormPost $ projectFormlet Nothing
      defaultLayout $ do
                      setTitle "Create New Project"
                      let objName :: Text
                          objName = "Create Project"
                          actionName :: Text
                          actionName = "Create"
                      addWidget $(widgetFile "createEdit")
    False -> permissionDenied "Not Authorized"

postProjectCreateR :: Handler ()
postProjectCreateR = do
  uId <- requireAuthId
  profile <- runDB $ snd <$> getBy404 (UniqueProfile uId)
  case (isEditableType . profileType) profile of
    True -> do
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
    False -> permissionDenied "Not Authorized"

getProjectUserPermissionsR :: Text -> Handler RepHtml
getProjectUserPermissionsR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  let isValidUser p = (profileUser p /= uId) && (isEditableType . profileType $ p)
  allps <- runDB (filter isValidUser . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
  let names = map (fromString . Text.unpack . profileFullName) allps
      auths = map (Just . (`elem` owners) . profileUser) allps
      allUids = map profileUser allps
  ((res, form), enctype) <- runFormPost $ renderTable $ userAccessField allUids names auths
  defaultLayout $ do
    setTitle "Add Users"
    let heading = "Project Collaborators for " <++> projectName project
    addWidget $(widgetFile "getUsers")

postProjectUserPermissionsR :: Text -> Handler ()
postProjectUserPermissionsR name = do
  uId <- requireAuthId
  (prid, project, owners, _) <- getProjectAndOwnersOr404 name
  let isValidUser p = (profileUser p /= uId) && (isEditableType . profileType $ p)
  allps <- runDB (filter isValidUser . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
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
        objName = "Edit Project"
        actionName :: Text
        actionName = "Update"
    addWidget $(widgetFile "createEdit_divs")

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
  redirect RedirectTemporary ProjectsR

---------------------------
----- Helper functions ----
---------------------------

projectFormlet project = renderDivs $ Project
                         <$> areq textField "Name" (projectName <$> project)
                         <*> imageFieldReq "Project Image" (projectFullImage <$> project)
                         <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic slugFS (preEscapedText . projectSlug <$> project)))
                         <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic descFS (preEscapedText . projectDescription <$> project)))
    where descFS :: FieldSettings Text
          descFS = FieldSettings "Description" Nothing (Just "description") (Just "description")
          slugFS :: FieldSettings Text
          slugFS = FieldSettings "Slug" Nothing (Just "slug") (Just "slug")

getProjectAndOwnersOr404 prjName = do
    mret <- runDB $ getBy $ UniqueProject prjName
    case mret of
      Just (prid, project) -> do
                owners_ <- getOwners prid
                ownProfiles_ <- runDB $ mapM (((snd <$>)<$>) . getBy . UniqueProfile) owners_
                let (owners, ownProfiles) = unzip $ 
                                            filter (isEditableType . profileType . fromJust . snd) $ 
                                            filter (isJust . snd) $ zip owners_ ownProfiles_
                return (prid, project, owners, ownProfiles)
      _ -> notFound

getOwners prid = runDB (map (projectUserUser . snd) <$> selectList [ProjectUserProjectEq prid] [] 0 0)
