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
    | LosePotion       -- perde 1 poção do inventário (mín 0)
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
    | roll < 12 = evPotion
    | roll < 20 = evChildNpc
    | roll < 27 = evBard
    | roll < 34 = evGold "floresta"
    | roll < 41 = evScroll
    | roll < 47 = evHerbs
    | roll < 53 = evForestCircle
    | roll < 58 = evAbandonedVillage
    | roll < 63 = evUpsideRiver
    | roll < 68 = evBattlefield
    | roll < 73 = evUpsideSnow
    | roll < 78 = evForestMirror
    | roll < 83 = evTreeFaces
    | roll < 88 = evDisconnectionZone
    | roll < 92 = evIllusionExit
    | roll < 96 = evAmbush
    | roll < 99 = evIdentityMirror
    | otherwise = evIllusion

-- ── Eventos da Caverna ─────────────────────────────────────────────────────

pickCaveEvent :: Int -> ExploreEvent
pickCaveEvent roll
    | roll < 12 = evPotion
    | roll < 20 = evChildNpc
    | roll < 27 = evBard
    | roll < 34 = evGold "caverna"
    | roll < 41 = evScroll
    | roll < 47 = evHerbs
    | roll < 53 = evCaveEcho
    | roll < 58 = evDeepFootprints
    | roll < 63 = evCaveSilence
    | roll < 68 = evBattlefield
    | roll < 73 = evUpsideSnow
    | roll < 79 = evDisconnectionZone
    | roll < 84 = evIllusionExit
    | roll < 91 = evAmbush
    | roll < 96 = evIdentityMirror
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
    , evBody    = "Numa fissura no basalto, algumas moedas douradas brilham fracamente à luz da tocha. Cunhagem antiga, de um reino que talvez já não exista mais. Você as guarda sem saber se ainda valem algo."
    , evLog     = "Encontrou moedas antigas em uma fissura da caverna."
    , evOutcome = NormalEvent
    }
evGold _ = ExploreEvent
    { evIcon    = "💰"
    , evTitle   = "Moedas na terra"
    , evBody    = "Meio enterradas na lama úmida, algumas moedas de ouro reluzem entre as raízes. Alguém as perdeu há muito tempo, ou as deixou de propósito. O vínculo que as ligava ao dono foi cortado."
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

-- ── NPC ──────────────────────────────────────────────────────────────────────

evBard :: ExploreEvent
evBard = ExploreEvent
    { evIcon    = "🎶"
    , evTitle   = "O Bardo do Amanhã"
    , evBody    = "Sentado sobre uma pedra, um velho com um alaúde toca uma melodia estranhamente familiar. Você se aproxima e percebe que a letra fala de coisas que ainda não aconteceram: sua próxima batalha, um rosto que você ainda vai encontrar, uma escolha que ainda não foi feita. Ele não te olha. \"As músicas chegam antes dos eventos\", ele diz. \"Desde o Desligamento, só consigo cantar o que ainda não foi.\""
    , evLog     = "Encontrou um bardo que só canta eventos futuros, o tempo desvinculado pelo Desligamento."
    , evOutcome = NormalEvent
    }

-- ── Ambiente compartilhado ────────────────────────────────────────────────────

evBattlefield :: ExploreEvent
evBattlefield = ExploreEvent
    { evIcon    = "⚔"
    , evTitle   = "Campo de Batalha Eterno"
    , evBody    = "O chão está marcado por tochas extintas e armaduras enferrujadas. Mas os mortos ainda lutam. Soldados translúcidos avançam e recuam, cruzam espadas com inimigos igualmente transparentes, caem e se levantam, sem parar, sem perceber que a guerra acabou. O vínculo entre ação e consequência foi cortado. Eles repetem o combate por não conseguirem processar o fim."
    , evLog     = "Atravessou um campo de batalha onde os mortos ainda combatem em loop eterno."
    , evOutcome = NormalEvent
    }

evUpsideSnow :: ExploreEvent
evUpsideSnow = ExploreEvent
    { evIcon    = "❄"
    , evTitle   = "Neve ao Contrário"
    , evBody    = "Em uma área perfeitamente circular de dez metros, flocos de neve sobem. Não há vento, não há magia visível. A neve simplesmente desobedece à queda, flutuando para cima com calma absoluta. Você atravessa a fronteira e os flocos param, suspensos ao redor do seu rosto. Quando você sai do círculo, eles voltam a subir. Nenhuma explicação. Nenhum motivo. O Desligamento raramente precisa de um."
    , evLog     = "Encontrou uma área circular onde a neve cai para cima, gravidade desvinculada."
    , evOutcome = NormalEvent
    }

-- ── Ambiente exclusivo da Floresta ────────────────────────────────────────────

evForestMirror :: ExploreEvent
evForestMirror = ExploreEvent
    { evIcon    = "🪞"
    , evTitle   = "Espelho Sem Moldura"
    , evBody    = "No meio de uma clareira, sem apoio e sem moldura, um espelho paira no ar. Você se aproxima e seu reflexo te olha, mas não é hoje. As roupas são outras, o cansaço no rosto é diferente, e há uma ferida que você ainda não tem. O reflexo te observa por um momento. Então sorri levemente, como se soubesse de algo que você ainda vai descobrir. Quando você pisca, o espelho some."
    , evLog     = "Encontrou um espelho flutuante que mostrava seu reflexo de um dia diferente."
    , evOutcome = NormalEvent
    }

evTreeFaces :: ExploreEvent
evTreeFaces = ExploreEvent
    { evIcon    = "🌳"
    , evTitle   = "Árvores com Rostos"
    , evBody    = "Você para. Os troncos ao redor têm rostos. Não esculpidos, não gravados, são da própria madeira, como se sempre estivessem lá. Todos com a mesma expressão: confusos e com medo. Olhos abertos demais, bocas entreabertas, como se tivessem visto algo que não conseguem processar. Eles não se movem. Não falam. Só olham. Você caminha mais rápido."
    , evLog     = "Passou por uma floresta onde as árvores tinham rostos, confusos e com medo."
    , evOutcome = NormalEvent
    }

-- ── Armadilhas ────────────────────────────────────────────────────────────────

evDisconnectionZone :: ExploreEvent
evDisconnectionZone = ExploreEvent
    { evIcon    = "💀"
    , evTitle   = "Zona de Desligamento"
    , evBody    = "O ar muda de textura. Seus pés ficam pesados, seus pensamentos perdem coesão. Você atravessou uma Zona de Desligamento, uma área onde os vínculos internos começam a se desfazer. Seus próprios laços musculares e nervosos enfraquecem momentaneamente. Você sai do outro lado, mas algo ficou para trás."
    , evLog     = "Atravessou uma Zona de Desligamento. HP reduzido."
    , evOutcome = DrainHp 12
    }

evAmbush :: ExploreEvent
evAmbush = ExploreEvent
    { evIcon    = "⚔"
    , evTitle   = "Emboscada!"
    , evBody    = "Eles saem das sombras antes que você perceba. Criaturas do Desligamento, seres cujos vínculos com a razão foram cortados, restando só o instinto de destruir. Você recebe o primeiro golpe antes de poder reagir. Prepare-se para lutar enfraquecido."
    , evLog     = "Caiu em uma emboscada e entrou em batalha com HP reduzido."
    , evOutcome = StartAmbush 20
    }

evIllusion :: ExploreEvent
evIllusion = ExploreEvent
    { evIcon    = "✨"
    , evTitle   = "Ilusão de item"
    , evBody    = "Há brilho no chão que parece uma gema rara, pulsando suavemente. Você estende a mão. No instante em que seus dedos tocam, a ilusão colapsa e libera uma descarga de energia corrompida. Não havia nada real ali. Era uma armadilha de Desligamento, um falso vínculo criado para drenar quem o tocasse."
    , evLog     = "Tocou uma ilusão de item. A descarga de energia corrompida causou dano."
    , evOutcome = DrainHp 8
    }

evIllusionExit :: ExploreEvent
evIllusionExit = ExploreEvent
    { evIcon    = "🌀"
    , evTitle   = "Ilusão de Saída"
    , evBody    = "Você encontra um caminho que parece levar para fora, mas depois de muitos passos, reconhece a mesma árvore retorcida, a mesma pedra com musgo, o mesmo galho caído. Você andou em círculos por tempo indeterminado. O Desligamento criou um laço sem fim, um falso vínculo entre o caminho e o destino. Quando finalmente consegue quebrar o loop, percebe que um de seus pertences foi consumido pelo campo ilusório."
    , evLog     = "Preso em uma ilusão de saída. Andou em círculos e perdeu um item do inventário."
    , evOutcome = LosePotion
    }

evIdentityMirror :: ExploreEvent
evIdentityMirror = ExploreEvent
    { evIcon    = "🪞"
    , evTitle   = "Espelho de Identidade"
    , evBody    = "O espelho surge do nada, parado no caminho. Seu reflexo te olha, mas com seus atributos invertidos. Onde você é forte, ele é fraco. Onde você é lento, ele é veloz. É uma cópia corrompida de você mesmo, produto do Desligamento tentando criar um vínculo falso entre identidade e existência. O reflexo dá um passo para fora do espelho. Você não tem escolha."
    , evLog     = "Enfrentou um espelho de identidade. Combate contra uma versão invertida de si mesmo."
    , evOutcome = StartAmbush 0
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
            let (action, _) = case dest of
                    "caverna" -> (enterCave player,   "Caverna" :: Text)
                    _         -> (enterForest player, "Floresta" :: Text)
                (_, logs) = runGameM cfg action

            setSession "player-location" dest
            appendLogsToSession logs
            redirect ExploreR

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

        LosePotion -> do
            mPotTxt <- lookupSession "player-potions"
            let pots    = fromMaybe (0 :: Int) (mPotTxt >>= readMay . unpack)
                newPots = max 0 (pots - 1)
            setSession "player-potions" (tshow newPots)
            let msg = if pots > 0
                        then "🎒 Perdeu 1 Poção Pequena. Poções restantes: " <> tshow newPots
                        else "🎒 Sem itens no inventário para perder."
            renderNormalEvent cfg localeName evt (Just msg)

        StartAmbush dmg -> do
            (curHp, newHp) <- applyHpDrain dmg
            let msg = if dmg > 0
                        then "⚠ Você recebeu " <> tshow (curHp - newHp) <> " de dano antes de poder reagir. HP restante: " <> tshow newHp
                        else "⚠ Prepare-se para o combate."
            renderNormalEvent cfg localeName evt (Just msg)

-- Renderiza a tela de evento normal/positivo/dreno
renderNormalEvent :: GameConfig -> Text -> ExploreEvent -> Maybe Text -> Handler Html
renderNormalEvent cfg localeName evt mStatus = do
    mLoc <- lookupSession "player-location"
    let currentLocation :: Text
        currentLocation = fromMaybe "floresta" mLoc
    defaultLayout $ do
        setTitle "Exploração: Evento"
        $(widgetFile "explore/event")

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
