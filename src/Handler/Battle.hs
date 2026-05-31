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

-- ── Chaves de sessão para estado da batalha ───────────────────────────────────

keyPlayerHp :: Text
keyPlayerHp = "battle-player-hp"

keyEnemyHp :: Text
keyEnemyHp = "battle-enemy-hp"

keyRound :: Text
keyRound = "battle-round"

-- HP do jogador que persiste entre batalhas (inicializado na criação do personagem)
keyPersistHp :: Text
keyPersistHp = "player-hp"

keyPotions :: Text
keyPotions = "player-potions"

-- Lê o estado da batalha da sessão. Se não existir, cria um novo.
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
                enemy = buildEnemy cfg
            saveBattleState (BattleState php (enemyHp enemy) 0)
            pure $ BattleState php (enemyHp enemy) 0

saveBattleState :: BattleState -> Handler ()
saveBattleState bs = do
    setSession keyPlayerHp (tshow (bsPlayerHp bs))
    setSession keyEnemyHp  (tshow (bsEnemyHp  bs))
    setSession keyRound    (tshow (bsRound     bs))

clearBattleState :: Handler ()
clearBattleState = do
    deleteSession keyPlayerHp
    deleteSession keyEnemyHp
    deleteSession keyRound

-- ── GET /battle — mostra o estado atual da batalha ───────────────────────────

getBattleR :: Handler Html
getBattleR = withPlayerB $ \player cfg -> do
    -- Sempre começa uma batalha nova ao entrar via GET, mas preserva HP acumulado
    clearBattleState
    bs <- getBattleState player cfg
    let php   = bsPlayerHp bs
        enemy = buildEnemy cfg
    mPotTxt <- lookupSession keyPotions
    let potions = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)

    defaultLayout $ do
        setTitle "Combate"
        [whamlet|
            <div .hero>
                <h1>⚔ Combate
                <p .subtitle>#{worldName cfg} — #{weather cfg}

            <div .section-box>
                <h2>Inimigo encontrado!
                <p><strong>#{enemyName enemy}
                <p>HP do inimigo: #{show (enemyHp enemy)}
                <p>Seu HP: #{show php}
                <p>Seu poder de ataque: #{show (powerOf (playerClass player))}
                <p>Dano do inimigo por round: #{show (enemyPower cfg)}

            <div .section-box>
                <h2>O que deseja fazer?
                <div style="display:flex; gap:12px; align-items:center;">
                    <form method=post action=@{BattleR}>
                        ^{tokenWidget}
                        <input type=hidden name=acao value="atacar">
                        <button .btn .btn-primary type=submit>⚔ Atacar
                    <form method=post action=@{BattleR}>
                        ^{tokenWidget}
                        <input type=hidden name=acao value="fugir">
                        <button .btn type=submit>🏃 Fugir

            <div .section-box>
                <h2>🎒 Inventário
                $if potions > 0
                    <p>🧪 Poção Pequena (#{show potions}) — recupera 15 HP
                    <form method=post action=@{BattleR}>
                        ^{tokenWidget}
                        <input type=hidden name=acao value="pocao">
                        <button .btn type=submit>🧪 Usar Poção
                $else
                    <p>Inventário vazio.

            <div .section-box>
                <p><a href=@{LogsR}>Ver log da aventura
        |]

-- ── POST /battle — processa um round ─────────────────────────────────────────

postBattleR :: Handler Html
postBattleR = withPlayerB $ \player cfg -> do
    acao   <- runInputPost $ iopt textField "acao"
    mLoc   <- lookupSession "player-location"
    let locVal :: Text
        locVal   = fromMaybe "floresta" mLoc
        locLabel :: Text
        locLabel = case locVal of
                       "caverna" -> "🕳 Continuar na Caverna"
                       _         -> "🌲 Continuar na Floresta"

    case acao of
        -- ── Poção — cura sem gastar turno ────────────────────────────────────
        Just "pocao" -> do
            mPotTxt <- lookupSession keyPotions
            let potions = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
            if potions <= 0
                then redirect ExploreR
                else do
                    bs <- getBattleState player cfg
                    let maxHp  = initialPlayerHp (playerClass player)
                        healed = min 15 (maxHp - bsPlayerHp bs)
                        newHp  = bsPlayerHp bs + healed
                        newBs  = bs { bsPlayerHp = newHp }
                        newPots = potions - 1
                        enemy   = buildEnemy cfg
                        hdg :: Text
                        hdg     = if bsRound newBs == 0 then "⚔ Combate" else "⚔ Round " <> tshow (bsRound newBs)
                        logs    = ["🧪 " <> playerName player <> " usou uma Poção Pequena e recuperou " <> tshow healed <> " de HP!"]
                    saveBattleState newBs
                    setSession keyPotions (tshow newPots)
                    appendLogsToSession logs
                    defaultLayout $ do
                        setTitle "Combate"
                        [whamlet|
                            <div .hero>
                                <h1>#{hdg}
                                <p .subtitle>#{worldName cfg}

                            <div .section-box>
                                <h2>#{enemyName enemy}
                                <p>HP do inimigo: <strong>#{show (bsEnemyHp newBs)}
                                <p>Seu HP: <strong>#{show (bsPlayerHp newBs)}

                            <div .section-box>
                                <h2>Poção usada!
                                $forall entry <- logs
                                    <p .log-entry>-> #{entry}

                            <div .section-box>
                                <h2>O que deseja fazer?
                                <div style="display:flex; gap:12px; align-items:center;">
                                    <form method=post action=@{BattleR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=acao value="atacar">
                                        <button .btn .btn-primary type=submit>⚔ Atacar
                                    <form method=post action=@{BattleR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=acao value="fugir">
                                        <button .btn type=submit>🏃 Fugir

                            <div .section-box>
                                <h2>🎒 Inventário
                                $if newPots > 0
                                    <p>🧪 Poção Pequena (#{show newPots}) — recupera 15 HP
                                    <form method=post action=@{BattleR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=acao value="pocao">
                                        <button .btn type=submit>🧪 Usar Poção
                                $else
                                    <p>Inventário vazio.
                        |]

        -- ── Fuga ─────────────────────────────────────────────────────────────
        Just "fugir" -> do
            mHpTxt <- lookupSession keyPlayerHp
            clearBattleState
            forM_ mHpTxt $ \hpTxt -> setSession keyPersistHp hpTxt
            let logs = ["Jogador fugiu do combate.", "A sabedoria também é uma forma de sobreviver."]
            appendLogsToSession logs
            defaultLayout $ do
                setTitle "Fugiu!"
                [whamlet|
                    <div .hero>
                        <h1>🏃 Você fugiu!
                        <p .subtitle>Às vezes recuar é a melhor estratégia.

                    <div .section-box>
                        $forall entry <- logs
                            <p .log-entry>-> #{entry}

                    <div .section-box>
                        <div style="display:flex; gap:12px; align-items:center;">
                            <form method=post action=@{ExploreR}>
                                ^{tokenWidget}
                                <input type=hidden name=destino value=#{locVal}>
                                <button .btn .btn-primary type=submit>#{locLabel}
                            <a .btn href=@{ExploreR}>Escolher outro local
                        <p><a href=@{LogsR}>Ver log completo
                |]

        -- ── Atacar — roda um round de combate ─────────────────────────────────
        _ -> do
            bs <- getBattleState player cfg
            let (newBs, result, logs) = runRoundPure cfg player bs
            appendLogsToSession logs

            case result of
                -- Batalha continua
                Ongoing -> do
                    saveBattleState newBs
                    mPotTxt <- lookupSession keyPotions
                    let potions = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
                        enemy   = buildEnemy cfg
                    defaultLayout $ do
                        setTitle "Combate — Round em andamento"
                        [whamlet|
                            <div .hero>
                                <h1>⚔ Round #{show (bsRound newBs)}
                                <p .subtitle>#{worldName cfg}

                            <div .section-box>
                                <h2>#{enemyName enemy}
                                <p>HP do inimigo: <strong>#{show (bsEnemyHp newBs)}
                                <p>Seu HP: <strong>#{show (bsPlayerHp newBs)}

                            <div .section-box>
                                <h2>Log do round
                                $forall entry <- logs
                                    <p .log-entry>-> #{entry}

                            <div .section-box>
                                <h2>O que deseja fazer?
                                <div style="display:flex; gap:12px; align-items:center;">
                                    <form method=post action=@{BattleR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=acao value="atacar">
                                        <button .btn .btn-primary type=submit>⚔ Continuar atacando
                                    <form method=post action=@{BattleR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=acao value="fugir">
                                        <button .btn type=submit>🏃 Fugir

                            <div .section-box>
                                <h2>🎒 Inventário
                                $if potions > 0
                                    <p>🧪 Poção Pequena (#{show potions}) — recupera 15 HP
                                    <form method=post action=@{BattleR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=acao value="pocao">
                                        <button .btn type=submit>🧪 Usar Poção
                                $else
                                    <p>Inventário vazio.
                        |]

                -- Vitória — preserva HP restante para a próxima batalha
                Victory -> do
                    clearBattleState
                    setSession keyPersistHp (tshow (bsPlayerHp newBs))
                    defaultLayout $ do
                        setTitle "Vitória!"
                        [whamlet|
                            <div .hero>
                                <h1>🏆 Vitória!
                                <p .subtitle>Você derrotou o inimigo em #{show (bsRound newBs)} rounds.

                            <div .section-box>
                                <p>HP restante: <strong>#{show (bsPlayerHp newBs)}

                            <div .section-box>
                                <h2>Log do combate
                                $forall entry <- logs
                                    <p .log-entry>-> #{entry}

                            <div .section-box>
                                <div style="display:flex; gap:12px; align-items:center;">
                                    <form method=post action=@{ExploreR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=destino value=#{locVal}>
                                        <button .btn .btn-primary type=submit>#{locLabel}
                                    <a .btn href=@{ExploreR}>Escolher outro local
                                <p><a href=@{LogsR}>Ver log completo
                        |]

                -- Derrota — reseta HP para o valor inicial da classe
                Defeat -> do
                    clearBattleState
                    setSession keyPersistHp (tshow (initialPlayerHp (playerClass player)))
                    defaultLayout $ do
                        setTitle "Você foi derrotado"
                        [whamlet|
                            <div .hero>
                                <h1>💀 Você foi derrotado...
                                <p .subtitle>O #{show (bsRound newBs)}º round foi o seu último.

                            <div .section-box>
                                <h2>Log do combate
                                $forall entry <- logs
                                    <p .log-entry>-> #{entry}

                            <div .section-box>
                                <div style="display:flex; gap:12px; align-items:center;">
                                    <form method=post action=@{ExploreR}>
                                        ^{tokenWidget}
                                        <input type=hidden name=destino value=#{locVal}>
                                        <button .btn .btn-primary type=submit>#{locLabel}
                                    <a .btn href=@{ExploreR}>Escolher outro local
                                <p><a href=@{BattleR}>Tentar novamente
                                <p><a href=@{CharacterR}>Criar novo personagem
                                <p><a href=@{LogsR}>Ver log completo
                        |]

-- ── Helper: roda runGameM e extrai logs como lista pura ──────────────────────
--
-- Precisamos rodar o GameM (ReaderT + Writer) fora do Handler,
-- por isso extraímos tudo com runGameM aqui.

runRoundPure :: GameConfig -> Player -> BattleState -> (BattleState, BattleResult, [Text])
runRoundPure cfg player bs =
    let ((newBs, result), logs) = runGameM cfg (runRound player bs)
    in (newBs, result, logs)

-- ── Helpers de sessão e autenticação ─────────────────────────────────────────

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
                            <p><a href=@{CharacterR}>Criar personagem
                    |]
        _ -> defaultLayout [whamlet|
                <div .section-box>
                    <h1>Nenhum personagem encontrado
                    <p><a href=@{CharacterR}>Criar personagem
            |]

tokenWidget :: Widget
tokenWidget = do
    token <- liftHandler $ fmap reqToken getRequest
    case token of
        Nothing -> return ()
        Just t  -> toWidget [hamlet|<input type=hidden name=_token value=#{t}>|]