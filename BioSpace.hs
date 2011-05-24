{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module BioSpace
    ( BioSpace (..)
    , BioSpaceRoute (..)
    , resourcesBioSpace
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.HashDB
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import Model
import StaticFiles
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import qualified Data.Text as T

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data BioSpace = BioSpace
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnectionPool -- ^ Database connection pool.
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler BioSpace BioSpace

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget BioSpace BioSpace

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype BioSpaceRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route BioSpace = BioSpaceRoute
-- * Creates the value resourcesBioSpace which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- BioSpace. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the BioSpaceRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "BioSpace" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod BioSpace where
    approot _ = Settings.approot

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : T.unpack ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", T.pack fn] [], [])

-- How to run database actions.
instance YesodPersist BioSpace where
    type YesodDB BioSpace = SqlPersist
    runDB db = liftIOHandler
             $ fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodAuth BioSpace where
    type AuthId BioSpace = UserId
    loginDest _ = DashboardR
    logoutDest _ = RootR
    getAuthId = getAuthIdHashDB AuthR
    authPlugins = [authHashDB]
