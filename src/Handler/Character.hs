{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Character where

import Import
import Domain.Player
import Domain.World
import Domain.SessionLog

getCharacterR :: Handler Html
getCharacterR = do
    (formWidget, formEnctype) <- generateFormPost characterForm
    defaultLayout $ do
        setTitle "Criacao de Personagem"
        $(widgetFile "character")

postCharacterR :: Handler Html
postCharacterR = do
    ((result, formWidget), formEnctype) <- runFormPost characterForm
    case result of
        FormSuccess (nome, clsTxt, diffTxt) ->
            case (parsePlayerClass clsTxt, parseDifficulty diffTxt) of
                (Just cls, Just diff) -> do
                    let player = Player nome cls diff
                        cfg = gameConfigFromPlayer player

                    gameSaveId <- runDB $ insert GameSave
                        { gameSavePlayerName = playerName player
                        , gameSavePlayerClass = prettyPlayerClass (playerClass player)
                        , gameSaveDifficulty = prettyDifficulty (playerDifficulty player)
                        , gameSaveWorldName = worldName cfg
                        , gameSaveWeather = weather cfg
                        , gameSaveEnemyMultiplier = enemyMultiplier cfg
                        }

                    setSession "player-name" nome
                    setSession "player-class" clsTxt
                    setSession "player-difficulty" diffTxt
                    setSession "game-save-id" (toPathPiece gameSaveId)
                    clearLogsFromSession

                    defaultLayout $ do
                        setTitle "Personagem Criado"
                        [whamlet|
                            <div .hero>
                                <h1>Personagem criado com sucesso
                                <p .subtitle>
                                    Sua aventura em Monad Quest está pronta para começar.

                            <div .section-box>
                                <h2>Resumo do personagem
                                <p>
                                    <strong>Nome:</strong> #{playerName player}
                                <p>
                                    <strong>Classe:</strong> #{prettyPlayerClass (playerClass player)}
                                <p>
                                    <strong>Dificuldade:</strong> #{prettyDifficulty (playerDifficulty player)}

                            <div .section-box>
                                <h2>Configuração inicial do mundo
                                <p>
                                    <strong>Mundo:</strong> #{worldName cfg}
                                <p>
                                    <strong>Clima:</strong> #{weather cfg}
                                <p>
                                    <strong>Multiplicador de inimigos:</strong> #{show (enemyMultiplier cfg)}

                            <div .section-box>
                                <p>
                                    <a .btn .btn-primary href=@{ExploreR}>Iniciar exploração
                        |]
                _ ->
                    defaultLayout $ do
                        setTitle "Erro na criacao do personagem"
                        [whamlet|
                            <div .section-box>
                                <h1>Classe ou dificuldade invalida
                                <p>
                                    Os dados enviados pelo formulario nao puderam ser interpretados.
                                <p>
                                    <a .btn .btn-primary href=@{CharacterR}>Voltar para criacao de personagem
                        |]
        _ -> defaultLayout $ do
            setTitle "Criacao de Personagem"
            $(widgetFile "character")

characterForm :: Html -> MForm Handler (FormResult (Text, Text, Text), Widget)
characterForm = renderDivs $ (,,)
    <$> areq textField "Nome" Nothing
    <*> areq
        (selectFieldList classes)
        "Classe"
        Nothing
    <*> areq
        (selectFieldList difficulties)
        "Dificuldade"
        Nothing
  where
    classes =
        [ ("Guerreiro" :: Text, "warrior" :: Text)
        , ("Mago", "mage")
        , ("Ladino", "rogue")
        ]

    difficulties =
        [ ("Facil" :: Text, "easy" :: Text)
        , ("Media", "normal")
        , ("Dificil", "hard")
        ]