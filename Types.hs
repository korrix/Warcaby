{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Types where

import Linear.V2
import Data.Map.Strict
import Control.Lens

data FieldType = White | Black | WhiteQueen | BlackQueen deriving (Show, Read, Eq)

data Turn = Whites | Blacks deriving (Show, Read, Eq, Enum)

nextTurn :: Turn -> Turn
nextTurn Whites = Blacks
nextTurn Blacks = Whites

type Board = Map (V2 Int) FieldType

data GameState = GameState { _board :: Map (V2 Int) FieldType
                           , _selectedField :: Maybe (V2 Int)
                           , _turn :: Turn
                           } deriving (Show, Read, Eq)
makeLenses ''GameState

data Move = Ordinary (V2 Int) | Capture [V2 Int] deriving (Show, Read, Eq)

findMove :: V2 Int -> [Move] -> Maybe Move
findMove p (m@(Ordinary x):xs)      | p == x      = Just m
                                    | otherwise   = findMove p xs
findMove p (m@(Capture x@(_:_)):xs) | p `elem` x  = Just m
                                    | otherwise   = findMove p xs
findMove _ _                                      = Nothing

type PossibleMoves = Map (V2 Int) [Move]

vdir a b = pure signum <*> b - a
