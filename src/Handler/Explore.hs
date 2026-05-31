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
import Data.Unique (newUnique, hashUnique)

-- ── Evento de exploração aleatório ────────────────────────────────────────────

data ExploreEvent = ExploreEvent
    { evIcon      :: Text
    , evTitle     :: Text
    , evBody      :: Text
    , evLog       :: Text
    , evAddPotion :: Bool
    }

pickEvent :: Int -> Text -> ExploreEvent
pickEvent roll loc
    | roll < 25 = ExploreEvent
        { evIcon      = "🧪"
        , evTitle     = "Poção Pequena encontrada!"
        , evBody      = "Entre galhos e pedras, você encontra um frasco com um líquido rosado. Uma Poção Pequena foi adicionada ao seu inventário."
        , evLog       = "Encontrou uma Poção Pequena e a guardou no inventário."
        , evAddPotion = True
        }
    | roll < 45 = ExploreEvent
        { evIcon      = "💰"
        , evTitle     = "Moedas antigas!"
        , evBody      = goldDesc loc
        , evLog       = "Encontrou moedas antigas espalhadas pelo chão."
        , evAddPotion = False
        }
    | roll < 62 = ExploreEvent
        { evIcon      = "📜"
        , evTitle     = "Pergaminho misterioso"
        , evBody      = "Um pergaminho antigo com símbolos irreconhecíveis. Você o examina por alguns instantes, sem entender seu conteúdo."
        , evLog       = "Encontrou um pergaminho misterioso, mas não conseguiu decifrá-lo."
        , evAddPotion = False
        }
    | roll < 75 = ExploreEvent
        { evIcon      = "🦶"
        , evTitle     = "Pegadas estranhas..."
        , evBody      = footDesc loc
        , evLog       = "Encontrou pegadas de uma criatura desconhecida."
        , evAddPotion = False
        }
    | roll < 88 = ExploreEvent
        { evIcon      = "🌿"
        , evTitle     = "Plantas medicinais"
        , evBody      = "Você reconhece algumas ervas usadas por curandeiros. Infelizmente não sabe como prepará-las sem os utensílios certos."
        , evLog       = "Encontrou ervas medicinais, mas não soube utilizá-las."
        , evAddPotion = False
        }
    | otherwise = ExploreEvent
        { evIcon      = "🌑"
        , evTitle     = "Uma sensação estranha..."
        , evBody      = "O ambiente fica subitamente silencioso. A temperatura cai alguns graus. Nada acontece... por ora."
        , evLog       = "Sentiu uma presença inquietante, mas nada surgiu das sombras."
        , evAddPotion = False
        }

goldDesc :: Text -> Text
goldDesc "caverna" = "Numa fissura na rocha, algumas moedas douradas brilham fracamente na escuridão. Alguém esteve aqui antes de você."
goldDesc _         = "Meio enterradas na terra úmida, moedas de ouro reluzem entre as raízes. Uma herança esquecida pelo tempo."

footDesc :: Text -> Text
footDesc "caverna" = "Marcas profundas no solo da caverna. São grandes demais para serem humanas. Algo passou aqui recentemente."
footDesc _         = "Rastros pesados na lama da floresta. Uma criatura grande e desconhecida andou por aqui."

-- ── Helpers de sessão e autenticação ─────────────────────────────────────────

withPlayer :: (Player -> GameConfig -> Handler Html) -> Handler Html
withPlayer action = do
    mName  <- lookupSession "player-name"
    mClass <- lookupSession "player-class"
    mDiff  <- lookupSession "player-difficulty"
    case (mName, mClass, mDiff) of
        (Just nome, Just clsTxt, Just diffTxt) ->
            case (parsePlayerClass clsTxt, parseDifficulty diffTxt) of
                (Just cls, Just diff) ->
                    action (Player nome cls diff) (gameConfigFromPlayer (Player nome cls diff))
                _ -> errInvalidSession
        _ -> errNoSession

errNoSession :: Handler Html
errNoSession = defaultLayout [whamlet|
    <div .section-box>
        <h1>Nenhum personagem na sessão
        <p>Para explorar, primeiro crie um personagem.
        <p><a href=@{CharacterR}>Ir para criação de personagem
|]

errInvalidSession :: Handler Html
errInvalidSession = defaultLayout [whamlet|
    <div .section-box>
        <h1>Dados de personagem inválidos na sessão
        <p>Tente criar o personagem novamente.
        <p><a href=@{CharacterR}>Ir para criação de personagem
|]

-- ── GET /explore — escolha de local ──────────────────────────────────────────

getExploreR :: Handler Html
getExploreR = withPlayer $ \player cfg -> do
    defaultLayout $ do
        setTitle "Explorar"
        [whamlet|
            <div .hero>
                <h1>Explorar o mundo
                <p .subtitle>#{worldName cfg} — #{weather cfg}

            <div .section-box>
                <h2>Onde deseja ir, #{playerName player}?

                <div style="display:flex; gap:12px;">
                    <form method=post action=@{ExploreR}>
                        ^{tokenWidget}
                        <input type=hidden name=destino value="floresta">
                        <button .btn .btn-primary type=submit>
                            🌲 Entrar na floresta

                    <form method=post action=@{ExploreR}>
                        ^{tokenWidget}
                        <input type=hidden name=destino value="caverna">
                        <button .btn .btn-primary type=submit>
                            🕳 Explorar caverna

            <div .section-box>
                <p>
                    <a href=@{LogsR}>Ver log da aventura
        |]

-- ── POST /explore — destino ou evento aleatório ───────────────────────────────

postExploreR :: Handler Html
postExploreR = withPlayer $ \player cfg -> do
    acao    <- runInputPost $ iopt textField "acao"
    destino <- runInputPost $ iopt textField "destino"

    case acao of
        Just "explorar_local" -> handleExplorarLocal player cfg
        _                     -> handleDestino player cfg destino

-- Entra numa floresta ou caverna pela primeira vez
handleDestino :: Player -> GameConfig -> Maybe Text -> Handler Html
handleDestino player cfg mDestino = do
    let dest :: Text
        dest = fromMaybe "floresta" mDestino
        (action, localeName) = case dest of
            "caverna" -> (enterCave player,   "Caverna" :: Text)
            _         -> (enterForest player, "Floresta" :: Text)
        (description, logs) = runGameM cfg action

    setSession "player-location" dest
    appendLogsToSession logs

    defaultLayout $ do
        setTitle "Explorando"
        [whamlet|
            <div .hero>
                <h1>Explorando a #{localeName}
                <p .subtitle>#{worldName cfg}

            <div .section-box>
                <p>#{description}

            <div .section-box>
                <h2>Log desta exploração
                $forall entry <- logs
                    <p .log-entry>-> #{entry}

            <div .section-box>
                <div style="display:flex; gap:12px; align-items:center;">
                    <a .btn .btn-primary href=@{BattleR}>⚔ Encontrar inimigo
                    <form method=post action=@{ExploreR}>
                        ^{tokenWidget}
                        <input type=hidden name=acao value="explorar_local">
                        <button .btn type=submit>🔍 Explorar
                <p><a href=@{ExploreR}>Escolher outro local
                <p><a href=@{LogsR}>Ver log completo
        |]

-- Explora o local atual e sorteia um evento aleatório
handleExplorarLocal :: Player -> GameConfig -> Handler Html
handleExplorarLocal player cfg = do
    mLoc <- lookupSession "player-location"
    let loc :: Text
        loc       = fromMaybe "floresta" mLoc
        localeName :: Text
        localeName = case loc of
            "caverna" -> "Caverna"
            _         -> "Floresta"

    u <- liftIO newUnique
    let roll = abs (hashUnique u) `mod` 100 :: Int
        evt  = pickEvent roll loc

    when (evAddPotion evt) $ do
        mPotTxt <- lookupSession "player-potions"
        let pots = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
        setSession "player-potions" (tshow (pots + 1))

    appendLogsToSession [evLog evt]

    defaultLayout $ do
        setTitle "Exploração — Evento"
        [whamlet|
            <div .hero>
                <h1>#{evIcon evt} #{evTitle evt}
                <p .subtitle>#{localeName} — #{worldName cfg}

            <div .section-box>
                <p>#{evBody evt}
                <p .log-entry>-> #{evLog evt}

            <div .section-box>
                <div style="display:flex; gap:12px; align-items:center;">
                    <a .btn .btn-primary href=@{BattleR}>⚔ Encontrar inimigo
                    <form method=post action=@{ExploreR}>
                        ^{tokenWidget}
                        <input type=hidden name=acao value="explorar_local">
                        <button .btn type=submit>🔍 Explorar novamente
                <p><a href=@{ExploreR}>Escolher outro local
                <p><a href=@{LogsR}>Ver log completo
        |]

-- Widget auxiliar para o token CSRF
tokenWidget :: Widget
tokenWidget = do
    token <- liftHandler $ fmap reqToken getRequest
    case token of
        Nothing -> return ()
        Just t  -> toWidget [hamlet|<input type=hidden name=_token value=#{t}>|]
