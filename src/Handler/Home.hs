{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Settings.StaticFiles

data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        let heroBg = StaticR img_bg_monad_quest_png
            parchmentBg = StaticR img_aged_paper_png
            storyBg = StaticR img_story_bg_monad_png
        addStylesheetRemote "https://fonts.googleapis.com/css2?family=Pixelify+Sans:wght@400;500;700&display=swap"
        setTitle "Monad Quest"
        $(widgetFile "home/homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission =
            case result of
                FormSuccess res -> Just res
                _ -> Nothing
    allComments <- runDB getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        let heroBg = StaticR img_bg_monad_quest_png
            parchmentBg = StaticR img_aged_paper_png
            storyBg = StaticR img_story_bg_monad_png
        addStylesheetRemote "https://fonts.googleapis.com/css2?family=Pixelify+Sans:wght@400;500;700&display=swap"
        setTitle "Monad Quest"
        $(widgetFile "home/homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
  where
    textSettings =
        FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]