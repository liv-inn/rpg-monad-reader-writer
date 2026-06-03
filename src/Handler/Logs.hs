{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Logs where

import Import
import Domain.SessionLog
import Widgets.Help (helpWidget)

getLogsR :: Handler Html
getLogsR = do
    logs <- readLogsFromSession
    defaultLayout $ do
        setTitle "Monad Quest | Logs"
        addStylesheetRemote "https://fonts.googleapis.com/css2?family=Press+Start+2P&family=VT323&display=swap"
        helpWidget $(widgetFile "help/logs")
        $(widgetFile "logs/logs")