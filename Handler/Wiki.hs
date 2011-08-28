{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Wiki where

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

getPageR :: Text -> Handler RepHtml
getPageR name = do
  (pgid, page, owners, ownProfiles) <- getPageAndOwnersOr404 (humanizeName name)
  mu <- maybeAuthId
  isAdmin <- maybe (return False) checkAdmin mu
  let canEdit = maybe isAdmin ((|| isAdmin) . (`elem` owners)) mu
  defaultLayout $ do
    setTitle . toHtml $ "Genspace - " <++> (wikiPageName page)
    let content = addHtml (preEscapedText . wikiPageContents $ page)
    addWidget $(widgetFile "page")

getPageCreateR :: Handler RepHtml
getPageCreateR = do
  uId <- requireAuthId
  profile <- runDB $ snd <$> getBy404 (UniqueProfile uId)
  case (isEditableType . profileType) profile of
    True -> do
      ((res, form), enctype) <- runFormPost $ pageFormlet Nothing
      defaultLayout $ do
                   setTitle "Create New Page"
                   let objName :: Text
                       objName = "Create Page"
                       actionName :: Text
                       actionName = "Create"
                   addWidget $(widgetFile "createEdit")
    False -> permissionDenied "Not Authorized"

postPageCreateR :: Handler ()
postPageCreateR = do
  uId <- requireAuthId
  profile <- runDB $ snd <$> getBy404 (UniqueProfile uId)
  case (isEditableType . profileType) profile of
    True -> do
      ((res, form), enctype) <- runFormPost $ pageFormlet Nothing
      case res of
        FormSuccess page -> do
                    runDB $ do
                      pgid <- insert page
                      insert $ WikiPageUser pgid uId
                    redirect RedirectTemporary (PageR (wikiPageName page))
        FormFailure ts -> do
                    setMessage . toHtml $ join ", " ts
                    redirect RedirectTemporary PageCreateR
        _ -> redirect RedirectTemporary PageCreateR
    False -> permissionDenied "Not Authorized"

getPageUserPermissionsR :: Text -> Handler RepHtml
getPageUserPermissionsR name = do
  uId <- requireAuthId
  (pgid, page, owners, _) <- getPageAndOwnersOr404 name
  let isValidUser p = (profileUser p /= uId) && (isEditableType . profileType $ p)
  allps <- runDB (filter isValidUser . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
  let names = map (fromString . Text.unpack . profileFullName) allps
      auths = map (Just . (`elem` owners) . profileUser) allps
      allUids = map profileUser allps
  ((res, form), enctype) <- runFormPost $ renderTable $ userAccessField allUids names auths
  defaultLayout $ do
    setTitle "Add Users"
    let heading = "Page Collaborators for \"" <++> wikiPageName page <++> "\""
    addWidget $(widgetFile "getUsers")

postPageUserPermissionsR :: Text -> Handler ()
postPageUserPermissionsR name = do
  uId <- requireAuthId
  (pgid, page, owners, _) <- getPageAndOwnersOr404 name
  let isValidUser p = (profileUser p /= uId) && (isEditableType . profileType $ p)
  allps <- runDB (filter isValidUser . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
  let names = map (fromString . Text.unpack . profileFullName) allps
      auths = map (Just . (`elem` owners) . profileUser) allps
      allUids = map profileUser allps
  ((res, form), enctype) <- runFormPost $ renderTable $ userAccessField allUids names auths
  case res of
    FormSuccess auths -> do
             runDB $ mapM (\(uid,t) -> if t 
                                       then insert (WikiPageUser pgid uid) >> return ()
                                       else deleteBy (UniqueWikiPageUser uid pgid)
                          ) auths
             redirect RedirectTemporary (PageR name)
    FormFailure ts -> do
             setMessage . toHtml $ join ", " ts
             redirect RedirectTemporary (PageR name)
    _ -> redirect RedirectTemporary (PageR name)

-- /edit/page/#Text PageEditR GET POST
getPageEditR :: Text -> Handler RepHtml
getPageEditR name = do
  uId <- requireAuthId
  (pgid, page, owners, _) <- getPageAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ pageFormlet $ Just page
  defaultLayout $ do
    setTitle "Edit Page"
    let objName :: Text
        objName = "Edit Page"
        actionName :: Text
        actionName = "Update"
    addWidget $(widgetFile "createEdit_divs")

postPageEditR :: Text -> Handler ()
postPageEditR name = do
  uId <- requireAuthId
  (pgid, page, owners, _) <- getPageAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ pageFormlet $ Just page
  case res of
    FormSuccess page -> do
                       runDB $ replace pgid page
                       redirect RedirectTemporary (PageR (wikiPageName page))
    FormFailure ts -> do
                      setMessage . toHtml $ join ", " ts
                      redirect RedirectTemporary (PageEditR name)
    _ -> redirect RedirectTemporary (PageEditR name)

getPageDeleteR :: Text -> Handler RepHtml
getPageDeleteR name = do
  uId <- requireAuthId
  (pgid, page, owners, _) <- getPageAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Are You Sure?" (Just False)
  defaultLayout $ do
    setTitle "Page Delete Confirmation"
    addWidget $ [hamlet|
<h1> Deletion Confirmation - Page #{name}
<form enctype="#{enctype}" method=POST>
    ^{form}
    <input type="submit" value="Submit">
|]

postPageDeleteR :: Text -> Handler RepHtml
postPageDeleteR name = do
  uId <- requireAuthId
  (pgid, page, owners, _) <- getPageAndOwnersOr404 name
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Confirmed" (Just False)
  case res of
    FormSuccess True -> runDB $ do
                            -- delete all references to pgid
                            userPerms <- map fst <$> selectList [WikiPageUserPageEq pgid] [] 0 0
                            mapM_ delete userPerms
                            -- delete page itself
                            delete pgid
    FormFailure ts -> setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
    _ -> return ()
  redirect RedirectTemporary RootR

---------------------------
----- Helper functions ----
---------------------------

pageFormlet page = renderDivs $ WikiPage
                         <$> areq textField "Name" (wikiPageName <$> page)
                         <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic descFS (preEscapedText . wikiPageContents <$> page)))
    where descFS :: FieldSettings Text
          descFS = FieldSettings "Contents" Nothing (Just "pagecontents") (Just "pagecontents")

getPageAndOwnersOr404 pgName = do
    mret <- runDB $ getBy $ UniqueWikiPage pgName
    case mret of
      Just (pgid, page) -> do
                owners_ <- getOwners pgid
                ownProfiles_ <- runDB $ mapM (((snd <$>)<$>) . getBy . UniqueProfile) owners_
                let (owners, ownProfiles) = unzip $ 
                                            filter (isEditableType . profileType . fromJust . snd) $ 
                                            filter (isJust . snd) $ zip owners_ ownProfiles_
                return (pgid, page, owners, ownProfiles)
      Nothing -> notFound

getOwners pgid = runDB (map (wikiPageUserUser . snd) <$> selectList [WikiPageUserPageEq pgid] [] 0 0)

humanizeName = Text.replace "_" " "
