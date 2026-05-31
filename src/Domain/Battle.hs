{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Battle
    ( Enemy(..)
    , BattleResult(..)
    , BattleState(..)
    , initialPlayerHp
    , buildEnemy
    , buildEnemyM
    , powerOf
    , enemyPower
    , critChancePlayer
    , critChanceEnemy
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

-- Chance de crítico do jogador por dificuldade (0–99: se roll < chance, é crit)
critChancePlayer :: Difficulty -> Int
critChancePlayer Easy   = 30
critChancePlayer Normal = 20
critChancePlayer Hard   = 10

-- Chance de crítico do inimigo por dificuldade
critChanceEnemy :: Difficulty -> Int
critChanceEnemy Easy   = 5
critChanceEnemy Normal = 15
critChanceEnemy Hard   = 30

-- Constrói o inimigo com HP baseado no config
buildEnemy :: GameConfig -> Enemy
buildEnemy cfg = Enemy
    { enemyName = "Goblin das Sombras"
    , enemyHp   = 30 * enemyMultiplier cfg
    }

-- Versão monádica: usa Reader para ler o config e Writer para logar a criação
buildEnemyM :: GameM Enemy
buildEnemyM = do
    cfg <- ask
    let enemy = buildEnemy cfg
    tell ["Inimigo invocado: " <> enemyName enemy <> " (HP " <> tshow (enemyHp enemy) <> ")"]
    pure enemy

-- ── Lógica de um round ────────────────────────────────────────────────────────
--
-- Roda exatamente UM round de combate usando Reader (config) e Writer (log).
-- Retorna (BattleState atualizado, BattleResult).

-- playerRoll e enemyRoll são inteiros 0-99 gerados fora do GameM (no Handler via IO).
-- Se roll < critChance, o ataque é crítico (dano 2×).
runRound :: Player -> BattleState -> Int -> Int -> GameM (BattleState, BattleResult)
runRound player bs playerRoll enemyRoll = do
    cfg <- ask

    let diff         = baseDifficulty cfg
        enemy        = buildEnemy cfg
        isCritPlayer = playerRoll < critChancePlayer diff
        isCritEnemy  = enemyRoll  < critChanceEnemy  diff
        basePlayerAtk = powerOf (playerClass player)
        baseEnemyAtk  = enemyPower cfg
        playerAtk    = if isCritPlayer then basePlayerAtk * 2 else basePlayerAtk
        enemyAtk     = if isCritEnemy  then baseEnemyAtk  * 2 else baseEnemyAtk
        newEnemyHp   = max 0 (bsEnemyHp bs  - playerAtk)
        newPlayerHp  = max 0 (bsPlayerHp bs - enemyAtk)
        round_       = bsRound bs + 1
        critPlayerMsg = if isCritPlayer then " 💥 CRÍTICO!" else ""
        critEnemyMsg  = if isCritEnemy  then " 💥 CRÍTICO!" else ""

    tell
        [ "-- Round " <> tshow round_ <> " --"
        , playerName player <> " ataca por " <> tshow playerAtk <> " de dano." <> critPlayerMsg
        , "HP do inimigo: " <> tshow (bsEnemyHp bs) <> " -> " <> tshow newEnemyHp
        ]

    if newEnemyHp == 0
        then do
            tell ["Inimigo derrotado! Vitória de " <> playerName player <> "!"]
            pure (bs { bsEnemyHp = 0, bsRound = round_ }, Victory)
        else do
            tell
                [ enemyName enemy <> " revida com " <> tshow enemyAtk <> " de dano." <> critEnemyMsg
                , "HP do jogador: " <> tshow (bsPlayerHp bs) <> " -> " <> tshow newPlayerHp
                ]
            if newPlayerHp == 0
                then do
                    tell [playerName player <> " foi derrotado..."]
                    pure (bs { bsPlayerHp = 0, bsEnemyHp = newEnemyHp, bsRound = round_ }, Defeat)
                else do
                    tell ["Combate continua."]
                    pure (BattleState newPlayerHp newEnemyHp round_, Ongoing)