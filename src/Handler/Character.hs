{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Character where

import Import
    ( ($)
    , Show(show)
    , Applicative((<*>))
    , Maybe(Nothing, Just)
    , (<$>)
    , tshow
    , setSession
    , setTitle
    , whamlet
    , selectFieldList
    , textField
    , areq
    , generateFormPost
    , renderDivs
    , runFormPost
    , Html
    , PathPiece(toPathPiece)
    , PersistStoreWrite(insert)
    , Text
    , Yesod(defaultLayout)
    , Route(StaticR, ExploreR, CharacterR)
    , FormResult(FormSuccess)
    , MForm
    , YesodPersist(runDB)
    , widgetFile
    , GameSave(GameSave, gameSaveEnemyMultiplier, gameSavePlayerName,
               gameSavePlayerClass, gameSaveDifficulty, gameSaveWorldName,
               gameSaveWeather)
    , Widget
    , Handler
    , redirect,
    runInputPost,
      ireq,
    )
import Domain.Player
import Domain.World
    ( GameConfig(enemyMultiplier, worldName, weather),
      gameConfigFromPlayer )
import Domain.Battle (initialPlayerHp)
import Domain.SessionLog
import Settings.StaticFiles

getCharacterR :: Handler Html
getCharacterR = do
    (formWidget, formEnctype) <- generateFormPost characterForm
    defaultLayout $ do
        setTitle "Monad Quest | Character Creation"
        $(widgetFile "character/character")

postCharacterR :: Handler Html
postCharacterR = do
    nome    <- runInputPost $ ireq textField "name"
    clsTxt  <- runInputPost $ ireq textField "class"
    diffTxt <- runInputPost $ ireq textField "difficulty"

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
            setSession "player-hp" (tshow (initialPlayerHp cls))
            setSession "player-potions" "1"
            clearLogsFromSession

            redirect ExploreR

        _ ->
            defaultLayout $ do
                setTitle "Erro na criacao do personagem"
                [whamlet|
                    <div .section-box>
                        <h1>Classe ou dificuldade invalida
                        <p>Os dados enviados pelo formulario nao puderam ser interpretados.
                        <p>
                            <a .btn .btn-primary href=@{CharacterR}>Voltar para criacao de personagem
                |]

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