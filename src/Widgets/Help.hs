{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Widgets.Help
    ( helpWidget
    ) where

import Import
import Settings.StaticFiles

helpWidget :: Widget -> Widget
helpWidget inner = do
    addStylesheetRemote
        "https://fonts.googleapis.com/css2?family=Pixelify+Sans:wght@400;500;700&display=swap"

    toWidget [lucius|
        .help-fab {
            position: fixed;
            right: 20px;
            bottom: 20px;
            width: 64px;
            height: 64px;
            padding: 0;
            background: transparent;
            border: none;
            cursor: pointer;
            z-index: 9999;
            animation: helpFloat 3.2s ease-in-out infinite;
            transition: transform 0.12s ease, filter 0.12s ease;
        }

        .help-fab::before {
            content: "";
            position: absolute;
            inset: 8px;
            z-index: 0;
            border-radius: 999px;
            background: radial-gradient(
                circle,
                rgba(123, 211, 255, 0.22) 0%,
                rgba(157, 140, 255, 0.18) 38%,
                rgba(0, 0, 0, 0) 72%
            );
            filter: blur(10px);
            animation: helpPulse 2.8s ease-in-out infinite;
            pointer-events: none;
        }

        .help-fab:hover {
            filter: brightness(1.18)
                     drop-shadow(0 0 10px rgba(157,140,255,0.6));
        }

        .help-fab:active {
            transform: translateY(2px) scale(0.96);
        }

        .help-fab img {
            position: relative;
            z-index: 2;
            width: 48px;
            height: auto;
            display: block;
            margin: 0 auto;
            image-rendering: pixelated;
        }

        .spark {
            position: absolute;
            z-index: 1;
            width: 2px;
            height: 2px;
            border-radius: 999px;
            background: #eefcff;
            box-shadow:
                0 0 8px rgba(255, 255, 255, 0.95),
                0 0 16px rgba(123, 211, 255, 0.72),
                0 0 26px rgba(157, 140, 255, 0.32);
            pointer-events: none;
        }

        .spark-1 {
            top: 2px;
            left: 10px;
            animation: sparkleOne 2.8s ease-in-out infinite;
        }

        .spark-2 {
            top: 14px;
            right: 12px;
            animation: sparkleTwo 3.4s ease-in-out infinite;
        }

        .spark-3 {
            bottom: 12px;
            left: 16px;
            animation: sparkleThree 3.1s ease-in-out infinite;
        }

        .spark-4 {
            right: 6px;
            bottom: 10px;
            animation: sparkleFour 2.6s ease-in-out infinite;
        }

        .help-overlay {
            display: none;
            position: fixed;
            inset: 0;
            background: rgba(0, 0, 0, 0);
            z-index: 10000;
            align-items: center;
            justify-content: center;
            transition: background 0.35s ease;
        }

        .help-overlay.is-open {
            display: flex;
            background: rgba(0, 0, 0, 0.78);
            animation: overlayFadeIn 0.35s ease forwards;
        }

        .help-panel {
            position: relative;
            width: 820px;
            max-width: 95vw;
            min-height: 540px;
            background-image: url(/static/img/help-bttn/panel.png);
            background-size: 100% 100%;
            background-repeat: no-repeat;
            background-color: transparent;
            color: #3a2a1a;
            image-rendering: pixelated;
            animation: panelEntry 0.38s cubic-bezier(0.22, 1, 0.36, 1) forwards;
        }

        .help-panel-body {
            position: absolute;
            top: 100px;
            left: 104px;
            right: 104px;
            bottom: 88px;
            overflow-y: auto;
            font-family: "Pixelify Sans", sans-serif;
            color: #3a2a1a;
            text-align: left;
                scrollbar-width: none;

        }
        .help-panel-body::-webkit-scrollbar {
    width: 0;
    height: 0;
    }

        .help-panel-body h2,
        .help-panel-body p,
        .help-panel-body li,
        .help-panel-body span,
        .help-panel-body div {
            font-family: inherit;
        }

        .help-panel-body h2 {
            font-size: 1.75rem;
            font-weight: 700;
            color: #4b2f1f;
            margin: 0 0 0.9rem;
            letter-spacing: 0.04em;
            line-height: 1.2;
            animation: fadeSlideDown 0.4s ease 0.1s both;
            text-shadow: none;
        }

        .help-panel-body p {
            font-size: 1.2rem;
            font-weight: 400;
            line-height: 1.7;
            color: #5a4030;
            margin: 0 0 0.65rem;
            animation: fadeSlideDown 0.4s ease 0.2s both;
        }

        .help-close {
            position: absolute;
            top: 18px;
            right: 24px;
            background: transparent;
            border: none;
            color: #4b2f1f;
            font-size: 2rem;
            font-family: "Pixelify Sans", sans-serif;
            cursor: pointer;
            line-height: 1;
            padding: 0;
            transition: color 0.15s, transform 0.15s;
        }

        .help-close:hover {
            color: #2f1a12;
            transform: scale(1.2) rotate(10deg);
        }

        .help-mote {
            position: absolute;
            width: 3px;
            height: 3px;
            border-radius: 50%;
            background: rgba(120, 84, 48, 0.45);
            box-shadow: 0 0 6px rgba(120, 84, 48, 0.55);
            pointer-events: none;
        }

        .help-mote-1 {
            top: 18%;
            left: 12%;
            animation: moteFloat 5.2s ease-in-out infinite;
        }

        .help-mote-2 {
            top: 35%;
            right: 10%;
            animation: moteFloat 4.7s ease-in-out infinite 0.8s;
        }

        .help-mote-3 {
            bottom: 22%;
            left: 20%;
            animation: moteFloat 6.1s ease-in-out infinite 1.4s;
        }

        .help-mote-4 {
            bottom: 14%;
            right: 22%;
            animation: moteFloat 5.5s ease-in-out infinite 0.3s;
        }

        .help-mote-5 {
            top: 55%;
            left: 6%;
            animation: moteFloat 4.9s ease-in-out infinite 1.9s;
        }

        @keyframes overlayFadeIn {
            from { background: rgba(0, 0, 0, 0); }
            to   { background: rgba(0, 0, 0, 0.78); }
        }

        @keyframes panelEntry {
            from {
                opacity: 0;
                transform: scale(0.88) translateY(24px);
            }
            to {
                opacity: 1;
                transform: scale(1) translateY(0);
            }
        }

        @keyframes fadeSlideDown {
            from {
                opacity: 0;
                transform: translateY(-8px);
            }
            to {
                opacity: 1;
                transform: translateY(0);
            }
        }

        @keyframes moteFloat {
            0%, 100% {
                transform: translateY(0) scale(1);
                opacity: 0.4;
            }
            50% {
                transform: translateY(-14px) scale(1.3);
                opacity: 1;
            }
        }

        @keyframes helpFloat {
            0%, 100% { transform: translateY(0); }
            50%      { transform: translateY(-8px); }
        }

        @keyframes helpPulse {
            0%, 100% {
                opacity: 0.35;
                transform: scale(0.92);
            }
            50% {
                opacity: 0.95;
                transform: scale(1.08);
            }
        }

        @keyframes sparkleOne {
            0% {
                transform: translateY(0) scale(0.7);
                opacity: 0.15;
            }
            50% {
                transform: translateY(-10px) scale(1.15);
                opacity: 1;
            }
            100% {
                transform: translateY(-18px) scale(0.85);
                opacity: 0.08;
            }
        }

        @keyframes sparkleTwo {
            0% {
                transform: translateY(0) scale(0.8);
                opacity: 0.2;
            }
            50% {
                transform: translateY(-8px) translateX(4px) scale(1.1);
                opacity: 0.95;
            }
            100% {
                transform: translateY(-16px) translateX(6px) scale(0.82);
                opacity: 0.08;
            }
        }

        @keyframes sparkleThree {
            0% {
                transform: translateY(0) scale(0.7);
                opacity: 0.12;
            }
            50% {
                transform: translateY(-9px) translateX(-2px) scale(1.08);
                opacity: 0.9;
            }
            100% {
                transform: translateY(-15px) translateX(-4px) scale(0.82);
                opacity: 0.08;
            }
        }

        @keyframes sparkleFour {
            0% {
                transform: translateY(0) scale(0.76);
                opacity: 0.14;
            }
            50% {
                transform: translateY(-11px) scale(1.18);
                opacity: 1;
            }
            100% {
                transform: translateY(-17px) scale(0.84);
                opacity: 0.1;
            }
        }
        .help-panel-body h3 {
    font-size: 1.3rem;
    font-weight: 700;
    color: #4b2f1f;
    margin: 1rem 0 0.55rem;
    line-height: 1.2;
}

.help-table-block {
    margin: 0 0 1rem;
}

.help-table {
    width: 100%;
    border-collapse: collapse;
    font-family: "Pixelify Sans", sans-serif;
    font-size: 1.15rem;
    color: #3a2a1a;
    background: rgba(255, 248, 235, 0.58);
    border: 1px solid rgba(75, 47, 31, 0.22);
    border-radius: 8px;
    overflow: hidden;
}

.help-table th,
.help-table td {
    padding: 0.65rem 0.8rem;
    text-align: left;
    border: 1px solid rgba(75, 47, 31, 0.16);
}

.help-table thead th {
    background: rgba(75, 47, 31, 0.88);
    color: #f7ead7;
    font-weight: 700;
}

.help-table tbody td:first-child {
    font-weight: 700;
    color: #4b2f1f;
    background: rgba(75, 47, 31, 0.06);
}

.help-table tbody tr:nth-child(even) td {
    background: rgba(75, 47, 31, 0.04);
}

.help-code-block {
    background: #150d06;
    border-left: 3px solid #7a5a3a;
    border-radius: 0 4px 4px 0;
    padding: 0.75rem 1rem;
    margin: 0.5rem 0 1.1rem;
    overflow-x: auto;
}

.help-code-block code {
    display: block !important;
    font-family: "Courier New", Courier, monospace !important;
    font-size: 1rem !important;
    color: #cbbfa8 !important;
    line-height: 1.65 !important;
    white-space: pre !important;
    background: transparent !important;
    padding: 0 !important;
    border-radius: 0 !important;
    border: none !important;
}

.help-cm {
    color: #6b8a6b;
    font-style: italic;
}
    |]

    toWidget [julius|
        document.addEventListener("DOMContentLoaded", function () {
            var fab = document.querySelector(".help-fab");
            var overlay = document.querySelector(".help-overlay");
            var close = document.querySelector(".help-close");

            function openModal() {
                overlay.style.display = "flex";
                requestAnimationFrame(function () {
                    overlay.classList.add("is-open");
                });
            }

            function closeModal() {
                overlay.classList.remove("is-open");
                setTimeout(function () {
                    overlay.style.display = "none";
                }, 350);
            }

            if (fab && overlay) {
                fab.addEventListener("click", openModal);
                overlay.addEventListener("click", function (e) {
                    if (e.target === overlay) {
                        closeModal();
                    }
                });
            }

            if (close) {
                close.addEventListener("click", closeModal);
            }

            document.addEventListener("keydown", function (e) {
                if (e.key === "Escape") {
                    closeModal();
                }
            });
        });
    |]

    [whamlet|
        <button .help-fab type="button" aria-label="Abrir ajuda">
            <span .spark .spark-1 aria-hidden="true">
            <span .spark .spark-2 aria-hidden="true">
            <span .spark .spark-3 aria-hidden="true">
            <span .spark .spark-4 aria-hidden="true">
            <img src=@{StaticR img_help_bttn_help_button_png} alt="Ajuda">

        <div .help-overlay role="dialog" aria-modal="true">
            <div .help-panel>
                <span .help-mote .help-mote-1 aria-hidden="true">
                <span .help-mote .help-mote-2 aria-hidden="true">
                <span .help-mote .help-mote-3 aria-hidden="true">
                <span .help-mote .help-mote-4 aria-hidden="true">
                <span .help-mote .help-mote-5 aria-hidden="true">
                <button .help-close type="button" aria-label="Fechar ajuda">×
                <div .help-panel-body>
                    ^{inner}
    |]