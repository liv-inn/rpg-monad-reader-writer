{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.SessionLog
    ( appendLogsToSession
    , readLogsFromSession
    , clearLogsFromSession
    ) where

import Import

sessionLogKey :: Text
sessionLogKey = "adventure-log"

appendLogsToSession :: [Text] -> Handler ()
appendLogsToSession newLogs = do
    existing <- lookupSession sessionLogKey
    let previousText = fromMaybe "" existing
        newText =
            if null newLogs
                then previousText
                else
                    let block = unlines newLogs
                    in if previousText == ""
                        then block
                        else previousText <> "\n" <> block
    setSession sessionLogKey newText

readLogsFromSession :: Handler [Text]
readLogsFromSession = do
    existing <- lookupSession sessionLogKey
    pure $
        case existing of
            Nothing -> []
            Just txt -> filter (/= "") (lines txt)

clearLogsFromSession :: Handler ()
clearLogsFromSession = deleteSession sessionLogKey