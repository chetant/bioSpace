{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Commons where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Yesod.Auth
import Yesod.Form.Nic
import Yesod.Form.Jquery

import StaticFiles
import BioSpace

(<++>) = Text.append

mangleEmail :: Text -> Text
mangleEmail emailAddx = user <++> " < at > " <++> (Text.tail domain)
    where (user, domain) = Text.breakOn "@" emailAddx

checkAdmin uid = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ profileIsAdmin user

checkAuth pId uid = do
  (uId, user) <- runDB $ getBy404 $ UniqueProfile uid
  return $ (uId == pId) || (profileIsAdmin user)

join ch []  = ""
join ch [a] = a
join ch xs  = foldl1 (\a b -> a <++> ch <++> b) xs

profileFullName p = profileFirstName p <++> " " <++> profileLastName p

instance YesodNic BioSpace where
    urlNicEdit _ = Right "/static/js/nicEdit.js"

htmlFieldNic :: Field (GWidget sub BioSpace ()) FormMessage Html
htmlFieldNic = nicHtmlField

instance YesodJquery BioSpace where
    urlJqueryJs               _ = Left (StaticR js_jquery_min_js)
    urlJqueryUiJs             _ = Left (StaticR js_jquery_ui_min_js)
    urlJqueryUiCss            _ = Left (StaticR css_jquery_ui_css)
    urlJqueryUiDateTimePicker _ = Left (StaticR js_jquery_ui_datetimepicker_min_js)

dateTimeField = jqueryDayTimeField
