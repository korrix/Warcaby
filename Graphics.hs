{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Graphics where

import Prelude         as P
import Data.Map.Strict as M hiding (map)
import Diagrams.Prelude hiding (turn)
import Diagrams.Backend.Cairo (Cairo)

import Types
import Game

v2p :: (Integral a, Num n) => V2 a -> P2 n
v2p = p2 . unr2 . fmap fromIntegral

moveTo' :: (Integral a, Num (N t), HasOrigin t, V t ~ V2) => V2 a -> t -> t
moveTo' = moveTo . v2p

boardBackground :: Diagram Cairo
boardBackground = column [row [fyellow, fgreen], row [fgreen, fyellow]]
  where sq = square 1 # lw none
        fyellow   = sq # fc yellow
        fgreen    = sq # fc green
        alternate = concat . replicate 4
        row       = cat (r2 (1, 0)) . alternate
        column    = cat (r2 (0, 1)) . alternate

pawn :: V2 Int -> FieldType -> Diagram Cairo
pawn pos field = p # moveTo' pos
  where c  = if field == BlackQueen || field == Black      then red else white
        qc = if field == BlackQueen || field == WhiteQueen then black else c
        p  = circle 0.20 # scaleY 0.8 # fc qc
          <> circle 0.35 # scaleY 0.8 # fc c 
          <> circle 0.35 # scaleY 0.8 # fc black # translateY (-0.05)
          <> square 1    # lw none

boardDiagram :: Board -> Diagram Cairo
boardDiagram = foldrWithKey' (\k v acc -> pawn k v <> acc) mempty

possibleMovesDiagram :: Turn -> Board -> Diagram Cairo
possibleMovesDiagram t b = foldrWithKey drawArrows mempty (possibleMoves t b)
  where drawArrows _    [] acc = acc
        drawArrows src dst acc = mconcat (map (drawArrow src) dst) <> acc

        drawArrow :: V2 Int -> Move -> Diagram Cairo
        drawArrow (v2p -> src) = \case
          Ordinary (v2p -> dst)            -> arrowBetween src dst
          Capture (map v2p -> jumps@(_:_)) -> mconcat (map segment $ init arrows) 
                                           <> uncurry arrowBetween (last arrows)
            where arrows = zip (src : jumps) jumps
                  segment (s0, d0) = arrowBetween' (with & arrowHead .~ noHead) s0 d0
                                     <> circle 0.075 # fc black # moveTo d0
          _                                 -> mempty

gameDiagram :: GameState -> Diagram Cairo
gameDiagram (GameState b s t) = boardDiagram b
             <> possibleMovesDiagram t b
             <> case s of
                  Just s' -> square 1 # fc blue # lw none # moveTo' s'
                  Nothing -> mempty
             <> boardBackground
