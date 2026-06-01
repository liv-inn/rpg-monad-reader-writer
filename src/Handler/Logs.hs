{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Logs where

import Import
import Domain.SessionLog

getLogsR :: Handler Html
getLogsR = do
    logs <- readLogsFromSession
    defaultLayout $ do
        $(widgetFile "logs/logs")