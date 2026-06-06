# Monad Quest

Monad Quest é uma aplicação web em Haskell desenvolvida com Yesod para apresentar, de forma interativa, os conceitos das mônadas `Reader` e `Writer`.

Em vez de limitar a explicação a slides ou exemplos isolados, o projeto utiliza um RPG web como meio didático: cada tela do sistema relaciona elementos da jogabilidade com aspectos da composição monádica usados na implementação.

## Objetivo

O projeto foi desenvolvido como apoio a um seminário acadêmico sobre as mônadas `Reader` e `Writer`.

A proposta central é mostrar, na prática, como:

- `Reader` permite carregar uma configuração compartilhada e imutável ao longo da aplicação.
- `Writer` permite acumular logs e saídas suplementares durante a execução.
- A programação funcional pode ser usada para estruturar aplicações web com clareza e composicionalidade.

## Ideia do projeto

A aplicação simula uma pequena aventura em formato de RPG web.

Ao longo da experiência, o usuário:

- cria um personagem;
- define classe e dificuldade;
- explora o mundo;
- entra em combate;
- acompanha logs da execução;
- consulta painéis de ajuda que explicam como `Reader` e `Writer` aparecem na lógica do jogo.

Assim, o sistema não apenas utiliza os conceitos teoricamente, mas também os incorpora à própria interface como recurso pedagógico.

## Conceitos de mônadas no projeto

### Reader

A mônada `Reader` é usada para representar a leitura de um ambiente compartilhado, modelado no projeto por estruturas de configuração como `GameConfig`.

Esse ambiente pode conter informações como:

- dificuldade da partida;
- clima do mundo;
- multiplicadores de inimigo;
- parâmetros que afetam eventos e combate.

Com isso, diferentes partes do sistema conseguem consultar a mesma configuração sem precisar repassá-la manualmente por todos os níveis da aplicação.

### Writer

A mônada `Writer` é usada para registrar informações suplementares durante a execução, especialmente logs narrativos e operacionais do jogo.

Esses logs são utilizados para:

- registrar eventos da exploração;
- documentar decisões tomadas durante a execução;
- explicar ajustes aplicados por regras de dificuldade;
- exibir ao usuário um histórico da aventura.

## Funcionalidades

- Criação de personagem.
- Escolha de classe e dificuldade.
- Geração de configuração inicial do mundo.
- Exploração com eventos condicionados ao contexto do jogo.
- Sistema de batalha com rounds, poções e fuga.
- Registro de logs em sessão.
- Interface de ajuda contextual explicando o uso de `Reader` e `Writer`.
- Persistência de informações principais do jogador.

## Tecnologias utilizadas

- **Haskell**
- **Yesod**
- **mtl**
- **Persistent**
- **Stack**
- **Hamlet** para templates HTML
- **Lucius** para estilos CSS
- **Julius** para scripts JavaScript

## Estrutura geral

A aplicação está organizada em módulos responsáveis por aspectos distintos da experiência:

- `Handler.Home`: tela inicial.
- `Handler.Character`: criação de personagem.
- `Handler.Battle`: fluxo de combate.
- `Handler.Logs`: exibição dos logs acumulados.
- `Widgets.Help`: widget reutilizável de ajuda contextual.
- `Domain.*`: regras de negócio, mundo, combate, jogador e logs.

## Como executar localmente

### Pré-requisitos

- GHC
- Stack
- Dependências do projeto compatíveis com a versão configurada no `stack.yaml`

### Passos

```bash
stack build
stack exec -- yesod devel
```

Depois, abra a aplicação no endereço informado pelo servidor local.

## Build de produção

Para gerar uma versão de produção, utilize:

```bash
stack clean
stack build --flag haskell-a2:dev false
```

Em um deploy típico com Yesod scaffolded, os principais artefatos são:

- o executável da aplicação;
- a pasta `config/`;
- a pasta `static/`.

## Motivação acadêmica

Este projeto foi concebido como uma forma de transformar um conteúdo teórico de programação funcional em uma experiência visual e interativa.

A escolha por um RPG web permitiu associar elementos do jogo a decisões de modelagem funcional, tornando os conceitos de `Reader` e `Writer` mais concretos e intuitivos.

## Próximos passos

- realizar o deploy da aplicação;
- refinar a interface;
- expandir os eventos e fluxos do jogo;
- aprofundar a integração entre mecânica de jogo e explicações conceituais.
