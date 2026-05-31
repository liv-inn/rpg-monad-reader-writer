{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Story where

import Import

getStoryR :: Handler Html
getStoryR = defaultLayout $ do
    setTitle "O Reino de Lambda"
    [whamlet|
        <div .hero>
            <h1>O Reino de Lambda
            <p .subtitle>
                Antes de começar, você precisa entender onde está pisando.

        <div .section-box>
            <h2>O Mundo
            <p>
                Reino de Lambda é um mundo onde a magia funciona por
                <strong> vínculos</strong>
                . Toda criatura, feitiço e artefato existe
                <em> ligado a outro</em>
                . Nada age sozinho.
            <p>
                Um guerreiro sem causa perde a força.
                Um mago sem pergunta perde o feitiço.
                Um ladino sem segredo perde a sombra.

        <div .section-box>
            <h2>O Problema
            <p>
                Alguém está
                <strong> cortando os vínculos</strong>
                .
            <p>
                Florestas onde os animais pararam de se mover.
                Cavernas onde os ecos não respondem mais.
                Aldeias onde as pessoas acordam e não reconhecem os próprios nomes.
            <p>
                O fenômeno tem um nome sussurrado:
                <strong> o Desligamento</strong>
                .

        <div .section-box>
            <h2>A Verdade
            <p>
                Você descobre que o próprio sistema de magia de Lambda é uma
                <strong> construção</strong>
                . Alguém a programou.
            <p>
                E o Desligamento não é destruição:
                é alguém tentando
                <strong> reescrever as regras</strong>
                .

        <div .section-box>
            <h2>O Vilão
            <p>
                Ele não quer destruir o mundo.
            <p style="font-size:1.15rem; font-style:italic; color:#b05020;">
                Quer refatorá-lo.

        <div .section-box style="text-align:center; padding:2rem 1rem;">
            <p style="font-size:1.05rem; color:#555; margin-bottom:1.5rem;">
                Os vínculos ainda seguram. Por quanto tempo, ninguém sabe.
            <a .btn .btn-primary href=@{CharacterR}>
                Criar seu personagem →
    |]
