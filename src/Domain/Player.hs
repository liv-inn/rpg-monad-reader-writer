{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Player
    ( PlayerClass(..)
    , Difficulty(..)
    , Player(..)
    , parsePlayerClass
    , parseDifficulty
    , prettyPlayerClass
    , prettyDifficulty
    ) where

import Import

data PlayerClass
    = Warrior
    | Mage
    | Rogue
    deriving (Show, Eq)

data Difficulty
    = Easy
    | Normal
    | Hard
    deriving (Show, Eq)

data Player = Player
    { playerName       :: Text
    , playerClass      :: PlayerClass
    , playerDifficulty :: Difficulty
    }
    deriving (Show, Eq)

parsePlayerClass :: Text -> Maybe PlayerClass
parsePlayerClass cls =
    case toLower cls of
        "guerreiro" -> Just Warrior
        "warrior"   -> Just Warrior
        "mago"      -> Just Mage
        "mage"      -> Just Mage
        "ladino"    -> Just Rogue
        "rogue"     -> Just Rogue
        _           -> Nothing

parseDifficulty :: Text -> Maybe Difficulty
parseDifficulty d =
    case toLower d of
        "facil"  -> Just Easy
        "easy"   -> Just Easy
        "media"  -> Just Normal
        "média"  -> Just Normal
        "normal" -> Just Normal
        "dificil"-> Just Hard
        "difícil"-> Just Hard
        "hard"   -> Just Hard
        _        -> Nothing

prettyPlayerClass :: PlayerClass -> Text
prettyPlayerClass Warrior = "Guerreiro"
prettyPlayerClass Mage    = "Mago"
prettyPlayerClass Rogue   = "Ladino"

prettyDifficulty :: Difficulty -> Text
prettyDifficulty Easy   = "Fácil"
prettyDifficulty Normal = "Média"
prettyDifficulty Hard   = "Difícil"