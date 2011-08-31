{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Commons where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Yesod.Auth
import Yesod.Form.Nic
import Yesod.Form.Jquery
import Text.Feed.Import(parseFeedString)
import Text.Feed.Types
import Text.RSS.Syntax
import Data.Maybe(fromJust)
import Network.HTTP
import Network.URI(parseURI)

import StaticFiles
import BioSpace

(<++>) = Text.append

mangleEmail :: Text -> Text
mangleEmail emailAddx = user <++> " < at > " <++> (Text.tail domain)
    where (user, domain) = Text.breakOn "@" emailAddx

checkAdmin uid = do
  mret <- runDB $ getBy $ UniqueProfile uid
  case mret of 
    Just (uId, user) -> return $ profileIsAdmin user
    _ -> notFound

checkAuth pId uid = do
  mret <- runDB $ getBy $ UniqueProfile uid
  case mret of
    Just (uId, user) -> return $ (uId == pId) || (profileIsAdmin user)
    _ -> notFound

join ch []  = ""
join ch [a] = a
join ch xs  = foldl1 (\a b -> a <++> ch <++> b) xs

joinStr ch []  = ""
joinStr ch [a] = a
joinStr ch xs  = foldl1 (\a b -> a ++ ch ++ b) xs

trimStr [] = []
trimStr (' ':cs) = trimStr cs
trimStr cs = cs

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
dayField = jqueryDayField (def { jdsChangeMonth=True })

isEditableType :: UserType -> Bool
isEditableType Member = True
isEditableType CoFounder = True
isEditableType _ = False



data UpdateItem = UpdateItem {
      updateTitle :: String 
    , updateDesc :: String 
    , updateURL :: String
    } deriving (Show, Eq)

getUpdates :: IO [UpdateItem]
getUpdates = do
  feedStr <- getFeedFile "http://www.genspace.org/blog/feed"
  let itemToUpdateItem item = UpdateItem 
                              (fromJust . rssItemTitle $ item) 
                              (fromJust . rssItemDescription $ item) 
                              (fromJust . rssItemLink $ item)
      mfeed = parseFeedString feedStr
  return $ maybe [] (\(RSSFeed rss) -> (map itemToUpdateItem . rssItems . rssChannel) rss) mfeed

getFeedFile :: String -> IO String
getFeedFile url = do
  let request = Network.HTTP.Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = "" }
      uri = fromJust $ parseURI url
  resp <- simpleHTTP request
  case resp of
    Left x -> return ""
    Right r ->
        case rspCode r of
          (2,_,_) -> return $ rspBody r
          (3,_,_) -> maybe (return "") getFeedFile $ findHeader HdrLocation r
          _ -> return ""


class GridResource a where
    getImageUrl :: a -> Text
    getImageWidth :: a -> Text
    getImageHeight :: a -> Text
    getTitle :: a -> Text
    getUrl :: a -> Text

gridWidget :: (GridResource a, Monad m) => Int -> [a] -> GGWidget master m ()
gridWidget numCols [] = return ()
gridWidget numCols items = do
  let brkRows _ [] as = reverse as
      brkRows i (x:xs) (a:as)
          | i `mod` numCols == 0 = brkRows (i+1) xs ([x]:(a:as))
          | otherwise            = brkRows (i+1) xs ((x:a):as)
      brkRows i (x:xs) [] = brkRows (i+1) xs [[x]]
      mtx = brkRows 0 items []
      sample = head items
  addCassius $(cassiusFile "grid")
  addHamlet $(hamletFile "grid")
