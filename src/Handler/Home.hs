{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import
import Settings.StaticFiles

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    let heroBg = StaticR img_bg_monad_quest_png
   
    addStylesheetRemote "https://fonts.googleapis.com/css2?family=Pixelify+Sans:wght@400;500;700&display=swap"
    setTitle "Monad Quest"
    $(widgetFile "home/homepage")

postHomeR :: Handler Html
postHomeR = getHomeR