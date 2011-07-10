{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Event where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Lazy(toStrict)
import Data.Time
import System.Locale(defaultTimeLocale)
import Data.Char(isSpace)
import Text.Hamlet(renderHtmlText, preEscapedText)
import Data.String
import Yesod.Auth
import Yesod.Auth.HashDB(UserId, addUser, changePasswd)

import Fields.ImageUpload
import Fields.Users
import Handler.Commons
import StaticFiles
import BioSpace

getEventsR :: Handler RepHtml
getEventsR = getEventsViewR "week"

getEventsViewR :: Text -> Handler RepHtml
getEventsViewR viewRange = do
  currTime <- utcToLocalTime <$> liftIO getCurrentTimeZone <*> liftIO getCurrentTime
  let startDay = (getDateInt . localDay) currTime
      daysOffset = getDayOffset viewRange
      endDay = (getDateInt . (addDays daysOffset) . localDay) currTime
  getEventsBetR viewRange startDay endDay

getEventsFromR :: Text -> Int -> Handler RepHtml
getEventsFromR viewRange fromDate = do
  let daysOffset = getDayOffset viewRange
      toDate = (getDateInt . (addDays daysOffset) . dateFromYYYYMMDD) fromDate
  getEventsBetR viewRange fromDate toDate

getEventsBetR :: Text -> Int -> Int -> Handler RepHtml
getEventsBetR viewRange fromDate toDate = do
  mu <- maybeAuthId
  isAdmin <- maybe (return False) checkAdmin mu
  currTime <- utcToLocalTime <$> liftIO getCurrentTimeZone <*> liftIO getCurrentTime
  let startDay = dateFromYYYYMMDD fromDate
      endDay = dateFromYYYYMMDD toDate
  events <- map snd <$> (runDB $ selectList [EventDateGe startDay, EventDateLe endDay] [] 0 0)
  defaultLayout $ do
               setTitle "Genspace - Events"
               addScript $ StaticR js_jquery_min_js
               addScript $ StaticR js_jquery_ui_min_js
               addStylesheet $ StaticR css_jquery_ui_css
               addScript $ StaticR js_jquery_ui_datepicker_min_js
               addJulius $ [julius| 
                            $(function()
                              {
                                var startDate = $.datepicker.parseDate('yymmdd', "#{show fromDate}");
                                var endDate = $.datepicker.parseDate('yymmdd', "#{show toDate}");
                                $("#calendar").datepicker({
                                       dateFormat: "yymmdd",
                                       onSelect: function(dateText, inst)
                                       {
                                         window.location = "/events/#{viewRange}/from/"+dateText;
                                       },
                                       beforeShowDay: function(date)
                                       {
                                           if(startDate != null && endDate != null
                                              && date.getTime() >= startDate.getTime() 
                                              && date.getTime() <= endDate.getTime())
                                           {
                                             return [true, "highlighted"];
                                           }
                                           return [true, ""];
                                       }
                                });
                              })
                            |]
               addWidget $(widgetFile "events")

getEventR :: Int -> Int -> Text -> Handler RepHtml
getEventR dt tm title = do
  (evid, event, owners, ownProfiles) <- getEventAndOwnersOr404 dt tm title
  mu <- maybeAuthId
  isAdmin <- maybe (return False) checkAdmin mu
  let canEdit = maybe isAdmin (`elem` owners) mu
  defaultLayout $ do
    setTitle . toHtml $ "Genspace - Event - " <++> (eventTitle event)
    let description = addHtml (preEscapedText . eventDescription $ event)
    addWidget $(widgetFile "event")

getEventCreateR :: Handler RepHtml
getEventCreateR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ eventFormlet Nothing
  defaultLayout $ do
    setTitle "Create New Event"
    let objName :: Text
        objName = "Create Event"
        actionName :: Text
        actionName = "Create"
    addWidget $(widgetFile "createEdit")

postEventCreateR :: Handler ()
postEventCreateR = do
  uId <- requireAuthId
  ((res, form), enctype) <- runFormPost $ eventFormlet Nothing
  case res of
    FormSuccess event_ -> do
                       event <- runDB $ do
                                  let event = getEvent event_ uId
                                  eid <- insert event
                                  insert $ EventUser eid uId
                                  return event
                       redirect RedirectTemporary (EventR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))
    FormFailure ts -> do
                      setMessage . toHtml $ join ", " ts
                      redirect RedirectTemporary EventCreateR
    _ -> redirect RedirectTemporary EventCreateR

getEventUserPermissionsR :: Int -> Int -> Text -> Handler RepHtml
getEventUserPermissionsR dt tm title = do
  uId <- requireAuthId
  (evid, event, owners, _) <- getEventAndOwnersOr404 dt tm title
  allps <- runDB (filter ((/= uId) . profileUser) . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
  let names = map (fromString . Text.unpack . profileFullName) allps
      auths = map (Just . (`elem` owners) . profileUser) allps
      allUids = map profileUser allps
  ((res, form), enctype) <- runFormPost $ renderTable $ userAccessField allUids names auths
  defaultLayout $ do
    setTitle "Add Users"
    let heading = "Event Collaborators for " <++> eventTitle event
    addWidget $(widgetFile "getUsers")

postEventUserPermissionsR :: Int -> Int -> Text -> Handler ()
postEventUserPermissionsR dt tm title = do
  uId <- requireAuthId
  (evid, event, owners, _) <- getEventAndOwnersOr404 dt tm title
  allps <- runDB (filter ((/= uId) . profileUser) . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
  let names = map (fromString . Text.unpack . profileFullName) allps
      auths = map (Just . (`elem` owners) . profileUser) allps
      allUids = map profileUser allps
  ((res, form), enctype) <- runFormPost $ renderTable $ userAccessField allUids names auths
  case res of
    FormSuccess auths -> do
             runDB $ mapM (\(uid,t) -> if t 
                                       then insert (EventUser evid uid) >> return ()
                                       else deleteBy (UniqueEventUser uid evid)
                          ) auths
             redirect RedirectTemporary (EventR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))
    FormFailure ts -> do
             setMessage . toHtml $ join ", " ts
             redirect RedirectTemporary (EventR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))
    _ -> redirect RedirectTemporary (EventR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))

-- /edit/event/#Int EventEditR GET POST
getEventEditR :: Int -> Int -> Text -> Handler RepHtml
getEventEditR dt tm title = do
  uId <- requireAuthId
  (evid, event, owners, _) <- getEventAndOwnersOr404 dt tm title
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ eventFormlet $ Just event
  defaultLayout $ do
    setTitle "Edit Event"
    let objName :: Text
        objName = "Edit Event"
        actionName :: Text
        actionName = "Update"
    addWidget $(widgetFile "createEdit_divs")

postEventEditR :: Int -> Int -> Text -> Handler ()
postEventEditR dt tm title = do
  uId <- requireAuthId
  (evid, event, owners, _) <- getEventAndOwnersOr404 dt tm title
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ eventFormlet $ Just event
  case res of
    FormSuccess event_ -> do
             let event = getEvent event_ uId
             runDB $ replace evid event
             liftIO $ putStrLn $ "Obj:" ++ show event
             redirect RedirectTemporary (EventR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))
    FormFailure ts -> do
             setMessage . toHtml $ join ", " ts
             redirect RedirectTemporary (EventEditR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))
    _ -> redirect RedirectTemporary (EventEditR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))

getEventDeleteR :: Int -> Int -> Text -> Handler RepHtml
getEventDeleteR dt tm title = do
  uId <- requireAuthId
  (evid, event, owners, _) <- getEventAndOwnersOr404 dt tm title
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Are You Sure?" (Just False)
  defaultLayout $ do
    setTitle "Event Delete Confirmation"
    addWidget $ [hamlet|
<h1> Deletion Confirmation - Event #{eventTitle event}
<form enctype="#{enctype}" method=POST>
    ^{form}
    <input type="submit" value="Submit">
|]

postEventDeleteR :: Int -> Int -> Text -> Handler RepHtml
postEventDeleteR dt tm title = do
  uId <- requireAuthId
  (evid, event, owners, _) <- getEventAndOwnersOr404 dt tm title
  isAdmin <- checkAdmin uId
  let canEdit = isAdmin  || (uId `elem` owners)
  unless canEdit $ permissionDenied "Not Authorized"
  ((res, form), enctype) <- runFormPost $ renderDivs $ areq boolField "Confirmed" (Just False)
  case res of
    FormSuccess True -> runDB $ do
                            -- delete all references to evid
                            userPerms <- map fst <$> selectList [EventUserEventEq evid] [] 0 0
                            mapM_ delete userPerms
                            -- delete event itself
                            delete evid
    FormFailure ts -> setMessage . toHtml $ foldr (\a b -> a <++> ", " <++> b) "" ts
    _ -> return ()
  redirect RedirectTemporary EventsR

---------------------------
----- Helper functions ----
---------------------------

data Event_ = Event_ Text UTCTime Text Bool (Maybe Double) deriving (Show, Eq)

getEvent (Event_ title datetime desc isPublic mprice) uId = 
    Event (utctDay datetime) (timeToTimeOfDay . utctDayTime $ datetime)
          isPublic title desc mprice uId

eventDateTime ev = UTCTime (eventDate ev) (timeOfDayToTime $ eventTime ev)
eventDateLocalTime ev = LocalTime (eventDate ev) (eventTime ev)

eventFormlet event = renderDivs $ Event_
                         <$> areq textField "Name" (eventTitle <$> event)
                         <*> areq dateTimeField "Date & Time" (eventDateTime <$> event)
                         <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic descFS (preEscapedText . eventDescription <$> event)))
                         <*> areq boolField "Event Is Public?" (eventIsPublic <$> event)
                         <*> aopt doubleField "Price" (eventPrice <$> event)
    where descFS :: FieldSettings Text
          descFS = FieldSettings "Description" Nothing (Just "description") (Just "description")

dateFromYYYYMMDD :: Int -> Day
dateFromYYYYMMDD yyyymmdd = fromGregorian yyyy mm dd
    where yyyy = fromIntegral $ yyyymmdd `div` 10000
          mm = (yyyymmdd `div` 100) `mod` 100
          dd = yyyymmdd `mod` 100

timeOfDayFromHHMM :: Int -> TimeOfDay
timeOfDayFromHHMM hhmm = TimeOfDay hh mm 0
    where hh = hhmm `div` 100
          mm = hhmm `mod` 100

getEventAndOwnersOr404 dt tm title = do
    let date = dateFromYYYYMMDD dt
        time = timeOfDayFromHHMM tm
    (evid, event) <- runDB $ getBy404 $ UniqueEvent date time title
    os <- runDB $ (map snd) <$> selectList [EventUserEventEq evid] [] 0 0
    let owners = map eventUserUser os
    ownProfiles <- runDB $ mapM (((snd <$>)<$>) . getBy . UniqueProfile) owners
    return (evid, event, owners, ownProfiles)

getOwners evid = runDB (map (eventUserUser . snd) <$> selectList [EventUserEventEq evid] [] 0 0)

getDateIntFromEvent :: Event -> Int
getDateIntFromEvent = getDateInt . eventDate

getTimeIntFromEvent :: Event -> Int
getTimeIntFromEvent = getTimeInt . eventTime

getDateInt :: Day -> Int
getDateInt day = yyyy * 10000 + mm*100 + dd
    where (yrI, mm, dd) = toGregorian day
          yyyy = fromIntegral yrI

getTimeInt :: TimeOfDay -> Int
getTimeInt (TimeOfDay hh mm _) = hh * 100 + mm

getFormattedTime = (formatTime defaultTimeLocale " %l:%M %p on %A %b %e, %Y") . eventDateLocalTime
getShortFormTime = formatTime defaultTimeLocale "%A, %B %e"

getDayOffset :: Text -> Integer
getDayOffset "week" = 7
getDayOffset "month" = 30
getDayOffset "year" = 365
getDayOffset _ = 7
