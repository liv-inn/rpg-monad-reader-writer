{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.World
    ( GameConfig(..)
    , GameLog
    , GameM
    , runGameM
    , defaultConfig
    , gameConfigFromPlayer
    , enterForest
    , enterCave
    ) where

import Import
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Domain.Player

data GameConfig = GameConfig
    { worldName       :: Text
    , baseDifficulty  :: Difficulty
    , weather         :: Text
    , enemyMultiplier :: Int
    }
    deriving (Show, Eq)

type GameLog = [Text]

type GameM a = ReaderT GameConfig (Writer GameLog) a

runGameM :: GameConfig -> GameM a -> (a, GameLog)
runGameM cfg action = runWriter (runReaderT action cfg)

defaultConfig :: GameConfig
defaultConfig = GameConfig
    { worldName       = "Reino de Lambda"
    , baseDifficulty  = Normal
    , weather         = "Céu nublado"
    , enemyMultiplier = 1
    }

gameConfigFromPlayer :: Player -> GameConfig
gameConfigFromPlayer player =
    let diff = playerDifficulty player
    in GameConfig
        { worldName       = "Reino de Lambda"
        , baseDifficulty  = diff
        , weather         = weatherFor diff
        , enemyMultiplier = enemyMultiplierFor diff
        }

weatherFor :: Difficulty -> Text
weatherFor Easy   = "Céu claro"
weatherFor Normal = "Céu nublado"
weatherFor Hard   = "Tempestade arcana"

enemyMultiplierFor :: Difficulty -> Int
enemyMultiplierFor Easy   = 1
enemyMultiplierFor Normal = 2
enemyMultiplierFor Hard   = 3

enterForest :: Player -> GameM Text
enterForest _ = do
    cfg <- ask
    let msg = mconcat
            [ "Voce entra na floresta do mundo "
            , worldName cfg
            , ", em dificuldade "
            , prettyDifficulty (baseDifficulty cfg)
            , ". "
            , "O vento sopra entre as arvores, e voce sente que ha algo observando."
            ]
    tell
        [ "Jogador entrou na floresta."
        , "Clima atual: " <> weather cfg
        ]
    pure msg

enterCave :: Player -> GameM Text
enterCave _ = do
    cfg <- ask
    let msg = mconcat
            [ "Voce desce para dentro da caverna do mundo "
            , worldName cfg
            , ". "
            , "A escuridao aumenta e o ar fica mais frio."
            ]
    tell
        [ "Jogador entrou na caverna."
        , "Multiplicador de inimigos atual: " <> tshow (enemyMultiplier cfg)
        ]
    pure msg