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
import System.Random (randomRIO)
import Settings.StaticFiles
import Widgets.Help (helpWidget)

keyPlayerHp :: Text
keyPlayerHp = "battle-player-hp"

keyEnemyHp :: Text
keyEnemyHp = "battle-enemy-hp"

keyRound :: Text
keyRound = "battle-round"

keyPersistHp :: Text
keyPersistHp = "player-hp"

keyPotions :: Text
keyPotions = "player-potions"

playerWinSprite :: PlayerClass -> Route App
playerWinSprite Warrior = StaticR img_battle_win_guerreira_win_png
playerWinSprite Mage    = StaticR img_battle_win_maga_win_png
playerWinSprite Rogue   = StaticR img_battle_win_ladina_win_png

playerLoseSprite :: PlayerClass -> Route App
playerLoseSprite Warrior = StaticR img_battle_lose_guerreira_lose_png
playerLoseSprite Mage    = StaticR img_battle_lose_maga_lose_png
playerLoseSprite Rogue   = StaticR img_battle_lose_ladina_lose_png

playerCombatSprite :: PlayerClass -> Route App
playerCombatSprite = playerWinSprite

playerAltText :: PlayerClass -> Text
playerAltText Warrior = "Sprite da Guerreira"
playerAltText Mage    = "Sprite da Maga"
playerAltText Rogue   = "Sprite da Ladina"

goblinCombatSprite :: Route App
goblinCombatSprite = StaticR img_battle_globin_globin_png

goblinWinSprite :: Route App
goblinWinSprite = StaticR img_battle_globin_globin_win_png

goblinLoseSprite :: Route App
goblinLoseSprite = StaticR img_battle_globin_globin_lose_png

getBattleState :: Player -> GameConfig -> Handler BattleState
getBattleState player cfg = do
    mPhp <- lookupSession keyPlayerHp
    mEhp <- lookupSession keyEnemyHp
    mRnd <- lookupSession keyRound
    case (mPhp >>= readMay . unpack, mEhp >>= readMay . unpack, mRnd >>= readMay . unpack) of
        (Just php, Just ehp, Just rnd) ->
            pure $ BattleState php ehp rnd
        _ -> do
            mSavedHp <- lookupSession keyPersistHp
            let php = case mSavedHp >>= readMay . unpack of
                          Just hp -> hp
                          Nothing -> initialPlayerHp (playerClass player)
                (enemy, eLogs) = runGameM cfg buildEnemyM
            appendLogsToSession eLogs
            saveBattleState (BattleState php (enemyHp enemy) 0)
            pure $ BattleState php (enemyHp enemy) 0

saveBattleState :: BattleState -> Handler ()
saveBattleState bs = do
    setSession keyPlayerHp (tshow (bsPlayerHp bs))
    setSession keyEnemyHp  (tshow (bsEnemyHp bs))
    setSession keyRound    (tshow (bsRound bs))

clearBattleState :: Handler ()
clearBattleState = do
    deleteSession keyPlayerHp
    deleteSession keyEnemyHp
    deleteSession keyRound

renderCombat :: Player -> GameConfig -> Text -> Text -> Int -> Int -> Int -> [Text] -> Bool -> Handler Html
renderCombat player cfg battleHeading enemyNameT enemyHpVal playerHpVal potions roundLogs continuing = do
    mLoc <- lookupSession "player-location"
    let currentLocation :: Text
        currentLocation = fromMaybe "floresta" mLoc

        battleSubtitle :: Text
        battleSubtitle = worldName cfg <> " — " <> weather cfg

        playerNameT :: Text
        playerNameT = playerName player

        attackLabel :: Text
        attackLabel = if continuing then "⚔ Continuar atacando" else "⚔ Atacar"

        playerImg :: Route App
        playerImg = playerCombatSprite (playerClass player)

        playerImgAlt :: Text
        playerImgAlt = playerAltText (playerClass player)

        enemyImg :: Route App
        enemyImg = goblinCombatSprite

        enemyImgAlt :: Text
        enemyImgAlt = "Sprite do Globin"

    defaultLayout $ do
        setTitle "Combate"
        helpWidget $(widgetFile "help/battle")
        $(widgetFile "battle/combat")

renderResult
    :: Player
    -> GameConfig
    -> Text
    -> Text
    -> Text
    -> [Text]
    -> Text
    -> Text
    -> Bool
    -> Int
    -> Bool
    -> Bool
    -> Bool
    -> Handler Html
renderResult player cfg resultIcon resultHeading resultSubtitle resultLogs locVal locLabel showHpRemaining hpRemaining showRetry showCreateChar isVictory = do
    mLoc <- lookupSession "player-location"
    let currentLocation :: Text
        currentLocation = fromMaybe "floresta" mLoc

        playerImg :: Route App
        playerImg =
            if isVictory
                then playerWinSprite (playerClass player)
                else playerLoseSprite (playerClass player)

        playerImgAlt :: Text
        playerImgAlt = playerAltText (playerClass player)

        enemyImg :: Route App
        enemyImg =
            if isVictory
                then goblinLoseSprite
                else goblinWinSprite

        enemyImgAlt :: Text
        enemyImgAlt = "Sprite do Globin"

    defaultLayout $ do
        setTitle (toHtml resultHeading)
        helpWidget $(widgetFile "help/battle")
        $(widgetFile "battle/result")

getBattleR :: Handler Html
getBattleR = withPlayerB $ \player cfg -> do
    clearBattleState
    bs <- getBattleState player cfg
    mPotTxt <- lookupSession keyPotions
    let enemy = buildEnemy cfg
        potions = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
    renderCombat player cfg
        "⚔ Combate"
        (enemyName enemy) (bsEnemyHp bs) (bsPlayerHp bs)
        potions [] False

postBattleR :: Handler Html
postBattleR = withPlayerB $ \player cfg -> do
    acao <- runInputPost $ iopt textField "acao"
    mLoc <- lookupSession "player-location"
    let locVal :: Text
        locVal = fromMaybe "floresta" mLoc

        locLabel :: Text
        locLabel = case locVal of
            "caverna" -> "🕳 Continuar na Caverna"
            _         -> "🌲 Continuar na Floresta"

    case acao of
        Just "pocao" -> handlePotion player cfg
        Just "fugir" -> handleFlee player cfg locVal locLabel
        _            -> handleAttack player cfg locVal locLabel

handlePotion :: Player -> GameConfig -> Handler Html
handlePotion player cfg = do
    mPotTxt <- lookupSession keyPotions
    let potions = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
    if potions <= 0
        then redirect ExploreR
        else do
            bs <- getBattleState player cfg
            let maxHp = initialPlayerHp (playerClass player)
                healed = min 15 (maxHp - bsPlayerHp bs)
                newHp = bsPlayerHp bs + healed
                newBs = bs { bsPlayerHp = newHp }
                newPots = potions - 1
                enemy = buildEnemy cfg
                hdg :: Text
                hdg =
                    if bsRound newBs == 0
                        then "⚔ Combate"
                        else "⚔ Round " <> tshow (bsRound newBs)
                logs =
                    [ "🧪 " <> playerName player
                        <> " usou uma Poção Pequena e recuperou "
                        <> tshow healed <> " HP!"
                    ]
            saveBattleState newBs
            setSession keyPotions (tshow newPots)
            appendLogsToSession logs
            renderCombat player cfg
                hdg
                (enemyName enemy) (bsEnemyHp newBs) (bsPlayerHp newBs)
                newPots logs True

handleFlee :: Player -> GameConfig -> Text -> Text -> Handler Html
handleFlee player cfg locVal locLabel = do
    mHpTxt <- lookupSession keyPlayerHp
    clearBattleState
    forM_ mHpTxt $ \hpTxt -> setSession keyPersistHp hpTxt
    let logs =
            [ "Jogador fugiu do combate."
            , "A sabedoria também é uma forma de sobreviver."
            ]
    appendLogsToSession logs
    renderResult player cfg
        "🏃" "Você fugiu!" "Às vezes recuar é a melhor estratégia."
        logs locVal locLabel
        False 0 False False False

handleAttack :: Player -> GameConfig -> Text -> Text -> Handler Html
handleAttack player cfg locVal locLabel = do
    bs <- getBattleState player cfg
    playerRoll <- liftIO $ randomRIO (0 :: Int, 99)
    enemyRoll  <- liftIO $ randomRIO (0 :: Int, 99)
    let (newBs, result, logs) = runRoundPure cfg player bs playerRoll enemyRoll
    appendLogsToSession logs

    case result of
        Ongoing -> do
            saveBattleState newBs
            mPotTxt <- lookupSession keyPotions
            let potions = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
                enemy = buildEnemy cfg
            renderCombat player cfg
                ("⚔ Round " <> tshow (bsRound newBs))
                (enemyName enemy) (bsEnemyHp newBs) (bsPlayerHp newBs)
                potions logs True

        Victory -> do
            clearBattleState
            setSession keyPersistHp (tshow (bsPlayerHp newBs))
            renderResult player cfg
                "🏆"
                ("Vitória! Round " <> tshow (bsRound newBs))
                "Você derrotou o inimigo."
                logs locVal locLabel
                True (bsPlayerHp newBs) False False True

        Defeat -> do
            clearBattleState
            setSession keyPersistHp (tshow (initialPlayerHp (playerClass player)))
            renderResult player cfg
                "💀"
                "Você foi derrotado..."
                ("O round " <> tshow (bsRound newBs) <> " foi o seu último.")
                logs locVal locLabel
                False 0 True True False

runRoundPure :: GameConfig -> Player -> BattleState -> Int -> Int -> (BattleState, BattleResult, [Text])
runRoundPure cfg player bs playerRoll enemyRoll =
    let ((newBs, result), logs) = runGameM cfg (runRound player bs playerRoll enemyRoll)
    in (newBs, result, logs)

withPlayerB :: (Player -> GameConfig -> Handler Html) -> Handler Html
withPlayerB action = do
    mName  <- lookupSession "player-name"
    mClass <- lookupSession "player-class"
    mDiff  <- lookupSession "player-difficulty"
    case (mName, mClass, mDiff) of
        (Just nome, Just clsTxt, Just diffTxt) ->
            case (parsePlayerClass clsTxt, parseDifficulty diffTxt) of
                (Just cls, Just diff) ->
                    action (Player nome cls diff) (gameConfigFromPlayer (Player nome cls diff))
                _ -> defaultLayout [whamlet|
                        <div .section-box>
                            <h1>Sessão inválida
                            <p>
                                <a href=@{CharacterR}>Criar personagem
                    |]
        _ -> defaultLayout [whamlet|
                <div .section-box>
                    <h1>Nenhum personagem encontrado
                    <p>
                        <a href=@{CharacterR}>Criar personagem
            |]

tokenWidget :: Widget
tokenWidget = do
    token <- liftHandler $ fmap reqToken getRequest
    case token of
        Nothing -> return ()
        Just t  -> toWidget [hamlet|<input type=hidden name=_token value=#{t}>|]