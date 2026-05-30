{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Battle
    ( Enemy(..)
    , simpleBattle
    ) where

import Import
import Control.Monad.Reader (ask)
import Control.Monad.Writer.Strict (tell)
import Domain.Player
import Domain.World

data Enemy = Enemy
    { enemyName :: Text
    , enemyHp   :: Int
    }
    deriving (Show, Eq)

simpleBattle :: Player -> GameM Text
simpleBattle player = do
    cfg <- ask

    let multiplier = enemyMultiplier cfg
        heroPower =
            case playerClass player of
                Warrior -> 14
                Mage    -> 16
                Rogue   -> 12
        enemyBaseHp = 10 * multiplier
        enemy = Enemy
            { enemyName = "Goblin das Sombras"
            , enemyHp = enemyBaseHp
            }
        damageDealt = heroPower
        enemyRemainingHp = max 0 (enemyHp enemy - damageDealt)
        resultText =
            if enemyRemainingHp == 0
                then mconcat
                    [ playerName player
                    , " derrotou o "
                    , enemyName enemy
                    , " em "
                    , worldName cfg
                    , "."
                    ]
                else mconcat
                    [ playerName player
                    , " atacou o "
                    , enemyName enemy
                    , ", mas a criatura ainda resiste."
                    ]

    tell
        [ "Combate iniciado."
        , "Clima de batalha: " <> weather cfg
        , "Inimigo encontrado: " <> enemyName enemy
        , "HP inicial do inimigo: " <> tshow (enemyHp enemy)
        , "Jogador atacou com poder " <> tshow damageDealt
        , "HP restante do inimigo: " <> tshow enemyRemainingHp
        ]

    if enemyRemainingHp == 0
        then tell ["Resultado: vitória do jogador."]
        else tell ["Resultado: inimigo ainda vivo."]

    pure resultText