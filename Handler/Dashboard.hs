{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Dashboard where

import Yesod.Auth
import Control.Applicative((<$>))
import Data.Maybe(catMaybes)

import BioSpace
import Handler.Event

getDashboardR :: Handler RepHtml
getDashboardR = do
  uId <- requireAuthId
  profile <- runDB $ snd <$> (getBy404 $ UniqueProfile uId)
  pages <- runDB $ do
                  userPageIds <- map (wikiPageUserPage . snd) <$> selectList [WikiPageUserUserEq uId] [] 0 0
                  catMaybes <$> mapM get userPageIds
  projects <- runDB $ do
                  userPrjIds <- map (projectUserProject . snd) <$> selectList [ProjectUserUserEq uId] [] 0 0
                  catMaybes <$> mapM get userPrjIds
  events <- runDB $ do
                  userEvtIds <- map (eventUserEvent . snd) <$> selectList [EventUserUserEq uId] [] 0 0
                  catMaybes <$> mapM get userEvtIds
  defaultLayout $ do
                  setTitle $ "Genspace - Dashboard"
                  addWidget $(widgetFile "dashboard")
