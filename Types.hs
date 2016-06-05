{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Types where

import Linear.V2
import Data.Map.Strict as M
import Control.Lens

data FieldType = White | Black | WhiteQueen | BlackQueen deriving (Show, Read, Eq)

data Turn = Whites | Blacks deriving (Show, Read, Eq, Enum)

nextTurn :: Turn -> Turn
nextTurn Whites = Blacks
nextTurn Blacks = Whites

turnFilter :: Turn -> Board -> Board
turnFilter t = M.filter (isTurn t)
  where isTurn Whites White      = True
        isTurn Whites WhiteQueen = True
        isTurn Blacks Black      = True
        isTurn Blacks BlackQueen = True
        isTurn _      _          = False

type Board = Map (V2 Int) FieldType

getScore :: Turn -> Board -> Int
getScore t b = (12-) $ size $ turnFilter (nextTurn t) b

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

vdir :: Num a => V2 a -> V2 a -> V2 a
vdir a b = pure signum <*> b - a

takeWhile1 :: (t -> Bool) -> [t] -> [t]
takeWhile1 _ []          =  []
takeWhile1 p (x:xs)
             | p x       =  x : takeWhile1 p xs
             | otherwise =  [x]
