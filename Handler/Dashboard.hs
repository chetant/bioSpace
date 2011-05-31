{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Dashboard where

import Yesod.Auth
import Control.Applicative((<$>))
import BioSpace

getDashboardR :: Handler RepHtml
getDashboardR = do
  uId <- requireAuthId
  mprofile <- runDB $ (snd <$>) <$> (getBy $ UniqueProfile uId)
  defaultLayout $ do
                  -- runDB
                  setTitle $ "Genspace - Dashboard"
                  addWidget $(widgetFile "dashboard")
