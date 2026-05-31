{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Battle
    ( Enemy(..)
    , BattleResult(..)
    , BattleState(..)
    , initialPlayerHp
    , buildEnemy
    , powerOf
    , enemyPower
    , runRound
    ) where

import Import
import Control.Monad.Reader (ask)
import Control.Monad.Writer.Strict (tell)
import Domain.Player
import Domain.World

-- ── Tipos ─────────────────────────────────────────────────────────────────────

data Enemy = Enemy
    { enemyName :: Text
    , enemyHp   :: Int
    } deriving (Show, Eq)

data BattleResult
    = Ongoing   -- combate continua
    | Victory   -- jogador venceu
    | Defeat    -- jogador morreu
    deriving (Show, Eq)

data BattleState = BattleState
    { bsPlayerHp :: Int
    , bsEnemyHp  :: Int
    , bsRound    :: Int
    } deriving (Show, Eq)

-- ── Constantes ────────────────────────────────────────────────────────────────

-- HP inicial do jogador varia por classe
initialPlayerHp :: PlayerClass -> Int
initialPlayerHp Warrior = 100
initialPlayerHp Mage    = 70
initialPlayerHp Rogue   = 80

-- Dano do jogador por classe
powerOf :: PlayerClass -> Int
powerOf Warrior = 18
powerOf Mage    = 22
powerOf Rogue   = 15

-- Dano do inimigo (base 8, escala com dificuldade via enemyMultiplier no config)
enemyPower :: GameConfig -> Int
enemyPower cfg = 8 * enemyMultiplier cfg

-- Constrói o inimigo com HP baseado no config
buildEnemy :: GameConfig -> Enemy
buildEnemy cfg = Enemy
    { enemyName = "Goblin das Sombras"
    , enemyHp   = 30 * enemyMultiplier cfg
    }

-- ── Lógica de um round ────────────────────────────────────────────────────────
--
-- Roda exatamente UM round de combate usando Reader (config) e Writer (log).
-- Retorna (BattleState atualizado, BattleResult).

runRound :: Player -> BattleState -> GameM (BattleState, BattleResult)
runRound player bs = do
    cfg <- ask

    let playerAtk   = powerOf (playerClass player)
        enemyAtk    = enemyPower cfg
        newEnemyHp  = max 0 (bsEnemyHp bs  - playerAtk)
        newPlayerHp = max 0 (bsPlayerHp bs - enemyAtk)
        round_      = bsRound bs + 1

    tell
        [ "-- Round " <> tshow round_ <> " --"
        , playerName player <> " ataca por " <> tshow playerAtk <> " de dano."
        , "HP do inimigo: " <> tshow (bsEnemyHp bs) <> " -> " <> tshow newEnemyHp
        ]

    if newEnemyHp == 0
        then do
            tell ["Inimigo derrotado! Vitória de " <> playerName player <> "!"]
            pure (bs { bsEnemyHp = 0, bsRound = round_ }, Victory)
        else do
            tell
                [ "Goblin das Sombras revida com " <> tshow enemyAtk <> " de dano."
                , "HP do jogador: " <> tshow (bsPlayerHp bs) <> " -> " <> tshow newPlayerHp
                ]
            if newPlayerHp == 0
                then do
                    tell [playerName player <> " foi derrotado..."]
                    pure (bs { bsPlayerHp = 0, bsEnemyHp = newEnemyHp, bsRound = round_ }, Defeat)
                else do
                    tell ["Combate continua."]
                    pure (BattleState newPlayerHp newEnemyHp round_, Ongoing)