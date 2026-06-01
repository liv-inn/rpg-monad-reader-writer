{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Story where

import Import
import Settings.StaticFiles

getStoryR :: Handler Html
getStoryR = defaultLayout $ do
    addStylesheetRemote "https://fonts.googleapis.com/css2?family=Cinzel:wght@500;600;700&family=Cormorant+Garamond:wght@400;500;600;700&display=swap"
    setTitle "O Reino de Lambda"
    $(widgetFile "story/story")