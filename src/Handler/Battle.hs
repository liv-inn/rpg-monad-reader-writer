{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Battle where

import Import
import Domain.Player
import Domain.World
import Domain.Battle
import Domain.SessionLog

getBattleR :: Handler Html
getBattleR = do
    mName  <- lookupSession "player-name"
    mClass <- lookupSession "player-class"
    mDiff  <- lookupSession "player-difficulty"

    case (mName, mClass, mDiff) of
        (Just nome, Just clsTxt, Just diffTxt) ->
            case (parsePlayerClass clsTxt, parseDifficulty diffTxt) of
                (Just cls, Just diff) -> do
                    let player = Player nome cls diff
                        cfg = gameConfigFromPlayer player
                        (description, logs) = runGameM cfg (simpleBattle player)

                    appendLogsToSession logs

                    defaultLayout $ do
                        $(widgetFile "battle")
                _ ->
                    defaultLayout [whamlet|
                        <div .section-box>
                            <h1>Dados de personagem inválidos
                            <p>
                                Tente criar o personagem novamente.
                            <p>
                                <a href=@{CharacterR}>Voltar para criação de personagem
                    |]
        _ ->
            defaultLayout [whamlet|
                <div .section-box>
                    <h1>Nenhum personagem na sessão
                    <p>
                        Crie um personagem antes de iniciar um combate.
                    <p>
                        <a href=@{CharacterR}>Ir para criação de personagem
            |]