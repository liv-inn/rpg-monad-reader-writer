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
import System.Random (randomRIO)
import Control.Monad.Reader (ask)
import Control.Monad.Writer.Strict (tell)

-- ── Tipos de eventos ──────────────────────────────────────────────────────────

data ExploreOutcome
    = NormalEvent
    | GainPotion
    | DrainHp Int      -- perde HP (mín 1), fica no local
    | StartAmbush Int  -- perde HP e vai pra batalha

data ExploreEvent = ExploreEvent
    { evIcon    :: Text
    , evTitle   :: Text
    , evBody    :: Text
    , evLog     :: Text
    , evOutcome :: ExploreOutcome
    }

-- ── Tabelas de eventos ────────────────────────────────────────────────────────
--
-- Range 0-99 dividido em:
--   0-19  (20%) eventos comuns positivos
--  20-63  (44%) eventos comuns neutros / NPC
--  64-83  (20%) eventos exclusivos do local
--  84-99  (16%) armadilhas  ← menos chance

pickEvent :: Int -> Text -> ExploreEvent
pickEvent roll "caverna" = pickCaveEvent  roll
pickEvent roll _         = pickForestEvent roll

-- Versão monádica: Reader lê a dificuldade e ajusta o roll; Writer loga a rolagem
pickEventM :: Int -> Text -> GameM ExploreEvent
pickEventM rawRoll loc = do
    cfg <- ask
    let bias = case baseDifficulty cfg of
                   Hard -> 15    -- empurra pra armadilhas
                   Easy -> (-10) -- empurra pra eventos positivos
                   _    -> 0
        roll = max 0 (min 99 (rawRoll + bias))
    tell ["Rolagem: " <> tshow rawRoll <> " (ajuste de dificuldade: " <> tshow bias <> " → " <> tshow roll <> ")"]
    pure (pickEvent roll loc)

-- ── Eventos da Floresta ────────────────────────────────────────────────────

pickForestEvent :: Int -> ExploreEvent
pickForestEvent roll
    | roll < 20 = evPotion
    | roll < 31 = evChildNpc
    | roll < 43 = evGold "floresta"
    | roll < 53 = evScroll
    | roll < 64 = evHerbs
    | roll < 71 = evForestCircle
    | roll < 77 = evAbandonedVillage
    | roll < 84 = evUpsideRiver
    | roll < 90 = evDisconnectionZone
    | roll < 95 = evAmbush
    | otherwise = evIllusion

-- ── Eventos da Caverna ─────────────────────────────────────────────────────

pickCaveEvent :: Int -> ExploreEvent
pickCaveEvent roll
    | roll < 20 = evPotion
    | roll < 31 = evChildNpc
    | roll < 43 = evGold "caverna"
    | roll < 53 = evScroll
    | roll < 64 = evHerbs
    | roll < 71 = evCaveEcho
    | roll < 77 = evDeepFootprints
    | roll < 84 = evCaveSilence
    | roll < 90 = evDisconnectionZone
    | roll < 95 = evAmbush
    | otherwise = evIllusion

-- ── Definição dos eventos ─────────────────────────────────────────────────

evPotion :: ExploreEvent
evPotion = ExploreEvent
    { evIcon    = "🧪"
    , evTitle   = "Poção Pequena encontrada!"
    , evBody    = "Entre galhos e pedras, você encontra um frasco com um líquido rosado ainda selado. Parece que alguém passou por aqui e não pôde levá-lo. Uma Poção Pequena foi adicionada ao seu inventário."
    , evLog     = "Encontrou uma Poção Pequena e a guardou no inventário."
    , evOutcome = GainPotion
    }

evChildNpc :: ExploreEvent
evChildNpc = ExploreEvent
    { evIcon    = "👧"
    , evTitle   = "Uma criança perdida"
    , evBody    = "Você encontra uma criança sentada sozinha, olhando para as próprias mãos. Ela levanta os olhos e diz, com voz vazia: \"Como é meu nome mesmo?\". Não é uma pergunta. É uma constatação. O Desligamento chegou aqui."
    , evLog     = "Encontrou uma criança que esqueceu o próprio nome, primeiro sinal do Desligamento."
    , evOutcome = NormalEvent
    }

evGold :: Text -> ExploreEvent
evGold "caverna" = ExploreEvent
    { evIcon    = "💰"
    , evTitle   = "Moedas na rocha"
    , evBody    = "Numa fissura no basalto, algumas moedas douradas brilham fracamente à luz da tocha. Cunhagem antiga — de um reino que talvez já não exista mais. Você as guarda sem saber se ainda valem algo."
    , evLog     = "Encontrou moedas antigas em uma fissura da caverna."
    , evOutcome = NormalEvent
    }
evGold _ = ExploreEvent
    { evIcon    = "💰"
    , evTitle   = "Moedas na terra"
    , evBody    = "Meio enterradas na lama úmida, algumas moedas de ouro reluzem entre as raízes. Alguém as perdeu há muito tempo — ou as deixou de propósito. O vínculo que as ligava ao dono foi cortado."
    , evLog     = "Encontrou moedas antigas enterradas na terra da floresta."
    , evOutcome = NormalEvent
    }

evScroll :: ExploreEvent
evScroll = ExploreEvent
    { evIcon    = "📜"
    , evTitle   = "Pergaminho cifrado"
    , evBody    = "Um rolo de couro com símbolos que pulsam levemente, como se respirassem. Você tenta ler, as letras reconhecem seu olhar e se reorganizam. Uma linha legível emerge: \"O vínculo entre causa e efeito foi desfeito aqui.\""
    , evLog     = "Encontrou um pergaminho com mensagem fragmentada sobre o Desligamento."
    , evOutcome = NormalEvent
    }

evHerbs :: ExploreEvent
evHerbs = ExploreEvent
    { evIcon    = "🌿"
    , evTitle   = "Ervas medicinais"
    , evBody    = "Você reconhece as folhas de cura-vínculo, usadas por curandeiros para restaurar laços internos. Infelizmente não tem os utensílios para prepará-las agora. Mas saber que ainda crescem aqui é um bom sinal."
    , evLog     = "Encontrou ervas medicinais, mas não soube utilizá-las sem os utensílios."
    , evOutcome = NormalEvent
    }

-- Floresta

evForestCircle :: ExploreEvent
evForestCircle = ExploreEvent
    { evIcon    = "🔄"
    , evTitle   = "Animais em círculo"
    , evBody    = "Você para. Num claro da floresta, uma dúzia de animais como lebres, raposas e um cervo, caminham em círculo perfeito, sem parar. Não parecem assustados. Não parecem famintos. Parecem... presos num loop. O vínculo que os ligava ao instinto foi cortado. Eles não vão parar por conta própria."
    , evLog     = "Encontrou animais presos em círculo, vínculo instintivo cortado pelo Desligamento."
    , evOutcome = NormalEvent
    }

evAbandonedVillage :: ExploreEvent
evAbandonedVillage = ExploreEvent
    { evIcon    = "🏚"
    , evTitle   = "Aldeia sem gente"
    , evBody    = "Uma aldeia intacta no meio da floresta. A lareira ainda quente. A mesa posta. Um gato dorme na soleira. Mas não há ninguém. Nenhum corpo, nenhum sinal de fuga. As pessoas simplesmente... desapareceram. Como se seus vínculos com o lugar tivessem sido apagados de uma vez."
    , evLog     = "Encontrou uma aldeia intacta e completamente vazia, nenhum sinal de fuga ou luta."
    , evOutcome = NormalEvent
    }

evUpsideRiver :: ExploreEvent
evUpsideRiver = ExploreEvent
    { evIcon    = "🌊"
    , evTitle   = "Rio que sobe"
    , evBody    = "Um rio atravessa a floresta, mas a água flui para cima. Lentamente, em silêncio, contra a gravidade. Não há magia visível, nenhum feitiço ativo. O vínculo gravitacional simplesmente foi cortado neste ponto. Você observa por um tempo, tentando encontrar algum sentido. Não encontra."
    , evLog     = "Encontrou um rio fluindo para cima, vínculo gravitacional cortado."
    , evOutcome = NormalEvent
    }

-- Caverna

evCaveEcho :: ExploreEvent
evCaveEcho = ExploreEvent
    { evIcon    = "🔊"
    , evTitle   = "Eco distorcido"
    , evBody    = "Você grita \"Olá!\" para testar o eco. A resposta vem, mas não é \"Olá\". É \"Saia\". Você tenta de novo, desta vez com seu nome. O eco responde com outro nome. Um nome que você não reconhece. O vínculo entre causa e resposta foi desfeito aqui. A caverna não ecoa mais, ela fala por conta própria."
    , evLog     = "A caverna respondeu com palavras diferentes, eco desvinculado pelo Desligamento."
    , evOutcome = NormalEvent
    }

evDeepFootprints :: ExploreEvent
evDeepFootprints = ExploreEvent
    { evIcon    = "🦶"
    , evTitle   = "Pegadas profundas"
    , evBody    = "Marcas no solo da caverna, fundas demais para serem humanas e largas demais para qualquer animal que você conhece. O rastro vem de dentro da rocha e desaparece de volta nela, como se a criatura pudesse atravessar a pedra. Você nota que as pegadas não têm dedos. Apenas uma forma vaga, como um vínculo mal resolvido."
    , evLog     = "Encontrou pegadas impossíveis que entram e saem da rocha sólida."
    , evOutcome = NormalEvent
    }

evCaveSilence :: ExploreEvent
evCaveSilence = ExploreEvent
    { evIcon    = "🌀"
    , evTitle   = "Silêncio absoluto"
    , evBody    = "De repente, nada. Seu passo não faz som. Sua respiração é muda. Você bate as mãos e não há nenhum ruído. Por alguns segundos longos, você existe num vácuo sonoro completo. Depois, lentamente, os sons voltam. Você não tem certeza se foram embora ou se você é que estava desconectado deles."
    , evLog     = "Experienciou um bolsão de silêncio absoluto, vínculo sonoro momentaneamente cortado."
    , evOutcome = NormalEvent
    }

-- ── Armadilhas ────────────────────────────────────────────────────────────────

evDisconnectionZone :: ExploreEvent
evDisconnectionZone = ExploreEvent
    { evIcon    = "💀"
    , evTitle   = "Zona de Desligamento"
    , evBody    = "O ar muda de textura. Seus pés ficam pesados, seus pensamentos perdem coesão. Você atravessou uma Zona de Desligamento — uma área onde os vínculos internos começam a se desfazer. Seus próprios laços musculares e nervosos enfraquecem momentaneamente. Você sai do outro lado, mas algo ficou para trás."
    , evLog     = "Atravessou uma Zona de Desligamento — HP reduzido."
    , evOutcome = DrainHp 12
    }

evAmbush :: ExploreEvent
evAmbush = ExploreEvent
    { evIcon    = "⚔"
    , evTitle   = "Emboscada!"
    , evBody    = "Eles saem das sombras antes que você perceba. Criaturas do Desligamento — seres cujos vínculos com a razão foram cortados, restando só o instinto de destruir. Você recebe o primeiro golpe antes de poder reagir. Prepare-se para lutar enfraquecido."
    , evLog     = "Caiu em uma emboscada e entrou em batalha com HP reduzido."
    , evOutcome = StartAmbush 20
    }

evIllusion :: ExploreEvent
evIllusion = ExploreEvent
    { evIcon    = "✨"
    , evTitle   = "Ilusão de item"
    , evBody    = "Há brilho no chão que parece uma gema rara, pulsando suavemente. Você estende a mão. No instante em que seus dedos tocam, a ilusão colapsa e libera uma descarga de energia corrompida. Não havia nada real ali. Era uma armadilha de Desligamento, um falso vínculo criado para drenar quem o tocasse."
    , evLog     = "Tocou uma ilusão de item de descarga de energia corrompida causou dano."
    , evOutcome = DrainHp 8
    }

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

-- ── GET /explore ──────────────────────────────────────────────────────────────

getExploreR :: Handler Html
getExploreR = withPlayer $ \player cfg -> do
    mLoc <- lookupSession "player-location"

    let currentLocation :: Text
        currentLocation = case mLoc of
            Just "floresta" -> "floresta"
            Just "caverna"  -> "caverna"
            _               -> "world"

        skyClass :: Text
        skyClass = case weather cfg of
            "sunny"  -> "sunny"
            "cloudy" -> "cloudy"
            "storm"  -> "storm"
            _        -> "cloudy"

        sceneTitle :: Text
        sceneTitle = case currentLocation of
            "floresta" -> "Floresta dos Vínculos"
            "caverna"  -> "Caverna do Eco Partido"
            _          -> "Explore o mundo"

        sceneSubtitle :: Text
        sceneSubtitle = case currentLocation of
            "floresta" ->
                "As árvores sussurram nomes esquecidos pelo Reino de Lambda."
            "caverna"  ->
                "A pedra devolve respostas que jamais foram perguntadas."
            _          ->
                "Escolha o próximo caminho sob o céu que rege esta jornada."

        classLabel :: Text
        classLabel = case playerClass player of
            Warrior -> "Guerreira"
            Mage    -> "Maga"
            Rogue   -> "Ladina"

        difficultyLabel :: Text
        difficultyLabel = case playerDifficulty player of
            Easy   -> "Fácil"
            Normal -> "Normal"
            Hard   -> "Difícil"

    defaultLayout $ do
        setTitle "Explorar"
        $(widgetFile "explore/explore")
-- ── POST /explore ─────────────────────────────────────────────────────────────

postExploreR :: Handler Html
postExploreR = withPlayer $ \player cfg -> do
    acao    <- runInputPost $ iopt textField "acao"
    destino <- runInputPost $ iopt textField "destino"

    case acao of
        Just "explorar_local" -> handleExplorarLocal player cfg
        _                     -> handleDestino player cfg destino

-- Entra num local pela primeira vez (ou troca de local)
handleDestino :: Player -> GameConfig -> Maybe Text -> Handler Html
handleDestino player cfg mDestino = do
    let dest :: Text
        dest = fromMaybe "" mDestino

    if dest == ""
        then do
            deleteSession "player-location"
            redirect ExploreR
        else do
            let (action, localeName) = case dest of
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

                    ^{actionButtons}
                |]

-- Explora o local atual — sorteia um evento aleatório
handleExplorarLocal :: Player -> GameConfig -> Handler Html
handleExplorarLocal player cfg = do
    mLoc <- lookupSession "player-location"
    let loc :: Text
        loc        = fromMaybe "floresta" mLoc
        localeName :: Text
        localeName = if loc == "caverna" then "Caverna" else "Floresta"

    roll <- liftIO (randomRIO (0, 99) :: IO Int)
    let (evt, rollLogs) = runGameM cfg (pickEventM roll loc)

    appendLogsToSession (rollLogs ++ [evLog evt])

    case evOutcome evt of
        NormalEvent -> renderNormalEvent cfg localeName evt Nothing

        GainPotion -> do
            mPotTxt <- lookupSession "player-potions"
            let pots = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
            setSession "player-potions" (tshow (pots + 1))
            renderNormalEvent cfg localeName evt
                (Just "🧪 Poção Pequena adicionada ao inventário!")

        DrainHp dmg -> do
            (curHp, newHp) <- applyHpDrain dmg
            let msg = "💀 Perdeu " <> tshow (curHp - newHp) <> " HP. HP atual: " <> tshow newHp
            renderNormalEvent cfg localeName evt (Just msg)

        StartAmbush dmg -> do
            (curHp, newHp) <- applyHpDrain dmg
            let actual = curHp - newHp
            defaultLayout $ do
                setTitle "Emboscada!"
                [whamlet|
                    <div .hero>
                        <h1>#{evIcon evt} #{evTitle evt}
                        <p .subtitle>#{localeName} — #{worldName cfg}

                    <div .section-box>
                        <p>#{evBody evt}
                        <p .log-entry>-> #{evLog evt}

                    <div .section-box style="border-left:4px solid #c0392b; background:#fff5f5;">
                        <p>⚠ Você recebeu <strong>#{show actual} de dano</strong> antes de poder reagir.
                        <p>HP restante: <strong>#{show newHp}

                    <div .section-box>
                        <div style="display:flex; gap:12px; align-items:center;">
                            <a .btn .btn-primary href=@{BattleR}>⚔ Lutar!
                            <a .btn href=@{ExploreR}>Tentar fugir (escolher outro local)
                        <p><a href=@{LogsR}>Ver log completo
                |]

-- Renderiza a tela de evento normal/positivo/dreno
renderNormalEvent :: GameConfig -> Text -> ExploreEvent -> Maybe Text -> Handler Html
renderNormalEvent cfg localeName evt mStatus =
    defaultLayout $ do
        setTitle "Exploração — Evento"
        [whamlet|
            <div .hero>
                <h1>#{evIcon evt} #{evTitle evt}
                <p .subtitle>#{localeName} — #{worldName cfg}

            <div .section-box>
                <p>#{evBody evt}
                <p .log-entry>-> #{evLog evt}

            $maybe status <- mStatus
                <div .section-box>
                    <p>#{status}

            ^{actionButtons}
        |]

-- Botões de ação comuns (Encontrar inimigo + Explorar novamente)
actionButtons :: Widget
actionButtons = [whamlet|
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

-- Drena HP (mínimo 1) e retorna (hpAnterior, hpNovo)
applyHpDrain :: Int -> Handler (Int, Int)
applyHpDrain dmg = do
    mHpTxt <- lookupSession "player-hp"
    let curHp = fromMaybe 30 (mHpTxt >>= readMay . unpack :: Maybe Int)
        newHp = max 1 (curHp - dmg)
    setSession "player-hp" (tshow newHp)
    pure (curHp, newHp)

-- Widget auxiliar para o token CSRF
tokenWidget :: Widget
tokenWidget = do
    token <- liftHandler $ fmap reqToken getRequest
    case token of
        Nothing -> return ()
        Just t  -> toWidget [hamlet|<input type=hidden name=_token value=#{t}>|]
