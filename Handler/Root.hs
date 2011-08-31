{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Root where

import Control.Applicative((<$>))
import Text.Hamlet(renderHtmlText, preEscapedText)
import StaticFiles
import BioSpace
import Handler.Commons

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- BioSpace.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  (projects :: [Project]) <- map snd <$> (runDB $ selectList [] [] 0 0)
  updates <- take 7 <$> liftIO getUpdates
  defaultLayout $ do
             h2id <- lift newIdent
             setTitle "Welcome to Genspace"
             addScript $ StaticR js_jquery_min_js
             addScript $ StaticR js_jquery_ui_min_js
             addStylesheet $ StaticR css_jquery_ui_css
             addScript $ StaticR js_jquery_cycle_all_min_js
             addWidget $(widgetFile "homepage")
