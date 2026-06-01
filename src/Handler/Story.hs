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
    let agedPaperBg = StaticR img_aged_paper_png
   
    addStylesheetRemote "https://fonts.googleapis.com/css2?family=Pixelify+Sans:wght@400;500;700&family=VT323&display=swap"
    setTitle "O Reino de Lambda"
    $(widgetFile "story/story")