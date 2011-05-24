{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Dashboard where

import BioSpace

getDashboardR :: Handler RepHtml
getDashboardR = do
  mu <- maybeAuth
  case mu of
    Nothing -> redirect RedirectTemporary (AuthR LoginR)
    Just (uid, _) ->
                  defaultLayout $ do
                       -- runDB
                       setTitle $ "Welcome back,"
                       addWidget $(widgetFile "homepage")
