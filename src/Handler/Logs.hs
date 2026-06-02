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
        helpWidget $(widgetFile "help/logs")
        $(widgetFile "logs/logs")