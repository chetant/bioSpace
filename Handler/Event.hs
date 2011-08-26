{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Event where

import Control.Applicative((<$>),(<*>))
import Control.Monad(when, unless)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Lazy(toStrict)
import Data.Time
import Data.Time.LocalTime.TimeZone.Series(TimeZoneSeries, localTimeToUTC')
import Data.Time.LocalTime.TimeZone.Olson(getTimeZoneSeriesFromOlsonFile)
import System.Locale(defaultTimeLocale)
import Data.Char(isSpace)
import Text.Hamlet(renderHtmlText, preEscapedText)
import Data.String
import Yesod.Auth
import Yesod.Auth.HashDB(UserId, addUser, changePasswd)
import qualified Data.Map as Map
import Data.Maybe(fromJust, isJust)

import Fields.ImageUpload
import Fields.Users
import Handler.Commons
import StaticFiles
import BioSpace

getEventsR :: Handler RepHtml
getEventsR = getEventsViewR "all"

getEventsViewR :: Text -> Handler RepHtml
getEventsViewR filterType = do
  currTime <- utcToLocalTime <$> liftIO getCurrentTimeZone <*> liftIO getCurrentTime
  let startDay = (getDateInt . localDay) currTime
      daysOffset = numDaysInYear
      endDay = (getDateInt . (addDays daysOffset) . localDay) currTime
  getEventsBetR filterType startDay endDay

getEventsFromR :: Text -> Int -> Handler RepHtml
getEventsFromR filterType fromDate = do
  let daysOffset = numDaysInYear
      toDate = (getDateInt . (addDays daysOffset) . dateFromYYYYMMDD) fromDate
  getEventsBetR filterType fromDate toDate

getEventsBetR :: Text -> Int -> Int -> Handler RepHtml
getEventsBetR filterType fromDate toDate = do
  mu <- maybeAuthId
  isAdmin <- maybe (return False) checkAdmin mu
  currTime <- utcToLocalTime <$> liftIO getCurrentTimeZone <*> liftIO getCurrentTime
  let calStartDay = localDay currTime
      calEndDay = ((addDays numDaysInYear) . localDay) currTime
      startDay = dateFromYYYYMMDD fromDate
      endDay = dateFromYYYYMMDD toDate
      toEntry (_, e) = (eventDate e, [e])
      isAuthorized (_,e) = (isJust mu) || (eventIsPublic e)
      evFilter = (buildEvTypeFilter filterType) . snd
  allEventsList <- (filter evFilter . filter isAuthorized) <$> (runDB $ selectList [EventDateGe calStartDay, EventDateLe calEndDay] [] 0 0)
  let eventsBetween = filter 
                      (\x -> (((eventDate . snd) x `diffDays` startDay) >= 0) && 
                             (((eventDate . snd) x `diffDays` endDay) <= 0))
                      allEventsList
      eventsList = map toEntry $ filter (not . eventTentative . snd) eventsBetween
      tentativeEventsList = map toEntry $ filter (eventTentative . snd) eventsBetween
  case (eventsList ++ tentativeEventsList) of
    [(_,[event])] -> redirect RedirectTemporary (EventR (getDateIntFromEvent event) (getTimeIntFromEvent event) (eventTitle event))
    otherwise -> do
      let events :: [[Event]]
          events = map snd $ Map.assocs $ foldr (uncurry $ Map.insertWith' (++)) Map.empty eventsList
          allEvents = map snd $ Map.assocs $ foldr (uncurry $ Map.insertWith' (++)) Map.empty $ eventsList
          outDaysStr = join ","  $ map (juliusifyDate . eventDate . head) $ filter ((buildEvTypeFilter "outdoors") . head) allEvents
          classDaysStr = join "," $ map (juliusifyDate . eventDate . head) $ filter ((buildEvTypeFilter "courses") . head) allEvents
          workDaysStr = join ","  $ map (juliusifyDate . eventDate . head) $ filter ((buildEvTypeFilter "workshops") . head) allEvents
          juliusifyDate date = "$.datepicker.parseDate('yymmdd', \"" <++> (Text.pack . show . getDateInt) date <++> "\")"
          slug event = addHtml (preEscapedText . eventSlug $ event)
          unscheduledEvents = map (head . snd) tentativeEventsList
      defaultLayout $ do
               setTitle "Genspace - Events"
               addScript $ StaticR js_jquery_min_js
               addScript $ StaticR js_jquery_ui_min_js
               addStylesheet $ StaticR css_jquery_ui_css
               addScript $ StaticR js_jquery_ui_datepicker_min_js
               addWidget $(widgetFile "events")

getEventR :: Int -> Int -> Text -> Handler RepHtml
getEventR dt tm title = do
  (evid, event, owners, ownProfiles) <- getEventAndOwnersOr404 dt tm title
  mu <- maybeAuthId
  isAdmin <- maybe (return False) checkAdmin mu
  let canEdit = maybe isAdmin (`elem` owners) mu
  currTime <- utcToLocalTime <$> liftIO getCurrentTimeZone <*> liftIO getCurrentTime
  let calStartDay = localDay currTime
      calEndDay = ((addDays numDaysInYear) . localDay) currTime
      fromDate = getDateInt calStartDay
      toDate = getDateInt calStartDay
      isAuthorized (_,e) = (isJust mu) || (eventIsPublic e)
      toEntry (_, e) = (eventDate e, [e])
  allEventsList <- filter isAuthorized <$> (runDB $ selectList [EventDateGe calStartDay, EventDateLe calEndDay] [] 0 0)
  let allEvents = map snd $ Map.assocs $ foldr (uncurry $ Map.insertWith' (++)) Map.empty $ map toEntry allEventsList
      outDaysStr = join ","  $ map (juliusifyDate . eventDate . head) $ filter ((buildEvTypeFilter "outdoors") . head) allEvents
      classDaysStr = join "," $ map (juliusifyDate . eventDate . head) $ filter ((buildEvTypeFilter "courses") . head) allEvents
      workDaysStr = join ","  $ map (juliusifyDate . eventDate . head) $ filter ((buildEvTypeFilter "workshops") . head) allEvents
      juliusifyDate date = "$.datepicker.parseDate('yymmdd', \"" <++> (Text.pack . show . getDateInt) date <++> "\")"
  defaultLayout $ do
    setTitle . toHtml $ "Genspace - Event - " <++> (eventTitle event)
    addScript $ StaticR js_jquery_min_js
    addScript $ StaticR js_jquery_ui_min_js
    addStylesheet $ StaticR css_jquery_ui_css
    addScript $ StaticR js_jquery_ui_datepicker_min_js
    let description = addHtml (preEscapedText . eventDescription $ event)
    addWidget $(widgetFile "event")

getEventCreateR :: Handler RepHtml
getEventCreateR = do
  uId <- requireAuthId
  profile <- runDB $ snd <$> getBy404 (UniqueProfile uId)
  case (isEditableType . profileType) profile of
    True -> do
        ((res, form), enctype) <- runFormPost $ eventFormlet Nothing
        defaultLayout $ do
                    setTitle "Create New Event"
                    let objName :: Text
                        objName = "Create Event"
                        actionName :: Text
                        actionName = "Create"
                    addWidget $(widgetFile "createEdit")
    False -> permissionDenied "Not Authorized"

postEventCreateR :: Handler ()
postEventCreateR = do
  uId <- requireAuthId
  profile <- runDB $ snd <$> getBy404 (UniqueProfile uId)
  case (isEditableType . profileType) profile of
    True -> do
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
    False -> permissionDenied "Not Authorized"

getEventUserPermissionsR :: Int -> Int -> Text -> Handler RepHtml
getEventUserPermissionsR dt tm title = do
  uId <- requireAuthId
  (evid, event, owners, _) <- getEventAndOwnersOr404 dt tm title
  let isValidUser p = (profileUser p /= uId) && (isEditableType . profileType $ p)
  allps <- runDB (filter isValidUser . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
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
  let isValidUser p = (profileUser p /= uId) && (isEditableType . profileType $ p)
  allps <- runDB (filter isValidUser . map snd <$> selectList [ProfileIsAdminEq False] [] 0 0)
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

data Event_ = Event_ Text UTCTime EventType (Maybe Text) Text Text Bool Double (Maybe Double) (Maybe Text) Bool deriving (Show, Eq)

getEvent (Event_ title datetime etype mimg slug desc isPublic duration mprice mlocation tentative) uId = 
    Event (utctDay datetime) (timeToTimeOfDay . utctDayTime $ datetime)
          etype isPublic title mimg slug desc duration mprice mlocation tentative uId

eventDateTime ev = UTCTime (eventDate ev) (timeOfDayToTime $ eventTime ev)
eventDateLocalTime ev = LocalTime (eventDate ev) (eventTime ev)

eventEndTime ev = timeToTimeOfDay $ timeOfDayToTime (eventTime ev) + duration
    where duration = secondsToDiffTime (floor $ (eventDuration ev) * 3600.0)

eventFormlet event = renderDivs $ Event_
                         <$> areq textField "Name" (eventTitle <$> event)
                         <*> areq dateTimeField "Date & Time" (eventDateTime <$> event)
                         <*> areq (selectField eventTypes) "Type" (eventType <$> event)
                         <*> imageFieldOpt "Icon Image" (eventIconImage <$> event)
                         <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic slugFS (preEscapedText . eventSlug <$> event)))
                         <*> (toStrict . renderHtmlText <$> (areq htmlFieldNic descFS (preEscapedText . eventDescription <$> event)))
                         <*> areq boolField "Event Is Public?" (eventIsPublic <$> event)
                         <*> areq doubleField "Duration(hours)" (eventDuration <$> event)
                         <*> aopt doubleField "Price" (eventPrice <$> event)
                         <*> aopt textField "Location" (eventLocation <$> event)
                         <*> areq boolField "Event is Tentative?" (eventTentative <$> event)
    where descFS :: FieldSettings Text
          descFS = FieldSettings "Description" Nothing (Just "description") (Just "description")
          slugFS :: FieldSettings Text
          slugFS = FieldSettings "Slug" Nothing (Just "slug") (Just "slug")
          eventTypes = [("Course", Class),("Talk/Workshop", Workshop)]

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
    mret <- runDB $ getBy $ UniqueEvent date time title
    case mret of
      Just (evid, event) -> do
                owners_ <- getOwners evid
                ownProfiles_ <- runDB $ mapM (((snd <$>)<$>) . getBy . UniqueProfile) owners_
                let (owners, ownProfiles) = unzip $ 
                                            filter (isEditableType . profileType . fromJust . snd) $ 
                                            filter (isJust . snd) $ zip owners_ ownProfiles_
                return (evid, event, owners, ownProfiles)
      _ -> notFound

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
getShortTime = formatTime defaultTimeLocale "%l:%M %p"

numDaysInYear = 365

getDayOffset :: Text -> Integer
getDayOffset "week" = 7
getDayOffset "month" = 30
getDayOffset "year" = numDaysInYear
getDayOffset _ = 7

rangeIsWeek "week" = True
rangeIsWeek _ = False
rangeIsMonth "month" = True
rangeIsMonth _ = False
rangeIsYear "year" = True
rangeIsYear _ = False

buildEvTypeFilter :: Text -> (Event -> Bool)
buildEvTypeFilter "courses" = (== Class) . eventType
buildEvTypeFilter "outdoors" = isJust . eventLocation
buildEvTypeFilter "workshops" = (== Workshop) . eventType
buildEvTypeFilter _ = const True

durationInHrs :: Event -> Int
durationInHrs = round . eventDuration

-- ICAL format gen functions

typeICAL :: ContentType
typeICAL = "text/calendar; charset=utf-8"

newtype RepICAL = RepICAL Content

instance HasReps RepICAL where
    chooseRep (RepICAL c) _ = return (typeICAL, c)

getICALR :: Handler RepICAL
getICALR = do
  mu <- maybeAuthId
  let isAuthorized (_,e) = (isJust mu) || (eventIsPublic e)
  events <- map snd <$> (runDB $ selectList [] [] 0 0)
  buildICAL events

buildICAL :: [Event] -> Handler RepICAL
buildICAL es = do
  -- TODO: get this reference timezone either per user, or change calendar to store events in UTC
  let es' = filter (not . eventTentative) es
  tzs <- liftIO $ getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/New_York"
  os <- map snd <$> (runDB $ mapM (getBy404 . UniqueProfile . eventOrganizer) es')
  getURL <- getUrlRender
  return $ RepICAL (toContent (calendar getURL tzs (zip os es')))
    where calendar :: (Route BioSpace -> Text) -> TimeZoneSeries -> [(Profile, Event)] -> Text
          calendar getURL tzs oes = 
               "BEGIN:VCALENDAR\n\
               \VERSION:2.0\n\
               \PRODID:-//bioSpace//genspace/NONSGML v1.0//EN\n"
               <++> foldr (showEvent getURL tzs) "END:VCALENDAR\n" oes
          showEvent getURL tzs (o, e) r =
               "BEGIN:VEVENT\n" <++>
               "UID:" <++> getUID e <++> "\n" <++>
               "DTSTAMP:" <++> eventTimeStr tzs e <++> "\n" <++>
               "ORGANIZER" <++> getOrganizerContact o <++> ":\n" <++>
               "DTSTART:" <++> eventTimeStr tzs e <++> "\n" <++>
               "DURATION:" <++> getEventDuration e <++> "\n" <++>
               "SUMMARY:" <++> eventTitle e <++> "\n" <++>
               "DESCRIPTION;ALTREP=\"" <++> eventURL <++> "\":Please click link or go to " <++> eventURL <++> " to access event details\n" <++>
               "URL:" <++> eventURL <++> "\n" <++>
               "END:VEVENT\n"
               <++> r
              where eventURL = getURL (EventR (getDateIntFromEvent e) (getTimeIntFromEvent e) (eventTitle e))
          getUID e = (Text.pack . show) (getDateIntFromEvent e) <++> (Text.pack . show) (getTimeIntFromEvent e) <++> eventTitle e <++> "@genspace.org"
          eventTimeStr tzs e = Text.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" (eventUTCTime tzs e)
          eventUTCTime tzs e = localTimeToUTC' tzs (eventDateLocalTime e)
          getOrganizerContact o = "CN=" <++> profileFullName o
          getEventDuration e = "PT" <++> (Text.pack . show) (durationInHrs e) <++> "H"
