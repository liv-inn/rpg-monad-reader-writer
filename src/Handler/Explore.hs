{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Explore where

import Import
import Domain.Player
import Domain.World
import Domain.SessionLog

getExploreR :: Handler Html
getExploreR = do
    mName  <- lookupSession "player-name"
    mClass <- lookupSession "player-class"
    mDiff  <- lookupSession "player-difficulty"

    case (mName, mClass, mDiff) of
        (Just nome, Just clsTxt, Just diffTxt) ->
            case (parsePlayerClass clsTxt, parseDifficulty diffTxt) of
                (Just cls, Just diff) -> do
                    let player = Player nome cls diff
                        cfg = gameConfigFromPlayer player
                        (description, logs) = runGameM cfg (enterForest player)

                    appendLogsToSession logs

                    defaultLayout $ do
                        $(widgetFile "explore")
                _ ->
                    defaultLayout [whamlet|
                        <div .section-box>
                            <h1>Dados de personagem inválidos na sessão
                            <p>
                                Tente criar o personagem novamente.
                            <p>
                                <a href=@{CharacterR}>Ir para criação de personagem
                    |]
        _ ->
            defaultLayout [whamlet|
                <div .section-box>
                    <h1>Nenhum personagem na sessão
                    <p>
                        Para explorar, primeiro crie um personagem.
                    <p>
                        <a href=@{CharacterR}>Ir para criação de personagem
            |]