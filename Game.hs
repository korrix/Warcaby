{-# LANGUAGE LambdaCase #-}
module Game where

import           Prelude hiding (lookup)
import qualified Data.Map.Strict as M

import Data.IORef
import Data.Maybe
import Control.Lens
import Control.Monad
import Linear.V2

import Types

isTurn :: Turn -> FieldType -> Bool
isTurn Whites White      = True
isTurn Whites WhiteQueen = True
isTurn Blacks Black      = True
isTurn Blacks BlackQueen = True
isTurn _      _          = False

initialGameState :: GameState
initialGameState = GameState (M.fromList $ whites ++ blacks) Nothing Whites
  where whites = map (flip (,) White) [ V2 1 0, V2 3 0, V2 5 0, V2 7 0
                                      , V2 0 1, V2 2 1, V2 4 1, V2 6 1
                                      , V2 1 2, V2 3 2, V2 5 2, V2 7 2
                                      ]
        blacks = map (flip (,) Black) [ V2 0 5, V2 2 5, V2 4 5, V2 6 5
                                       , V2 1 6, V2 3 6, V2 5 6, V2 7 6
                                       , V2 0 7, V2 2 7, V2 4 7, V2 6 7
                                      ]

updateGameState :: IORef GameState -> V2 Int -> IO ()
updateGameState gameStateRef p = do
  GameState board sf turn <- readIORef gameStateRef
  let pm = possibleMoves turn board
      save b s t = writeIORef gameStateRef $ GameState b s t

  case sf >>= flip M.lookup pm >>= findMove p of
    Just m -> save (movePawn board (fromJust sf) True m) Nothing (nextTurn turn)
    _      -> save board (Just p) turn

possibleMoves :: Turn -> Board -> PossibleMoves
possibleMoves t b = prioritize $ M.mapWithKey allFieldMoves $ M.filter (isTurn t) b
  where allFieldMoves p t = fieldMoves p t ++ fieldCaptures p t

        prioritize moves = M.map (filter $ \x -> priority x >= maxPriority) moves
          where maxPriority = maximum' $ M.map (maximum' . map priority) moves
                maximum' l = if null l then 0 else maximum l
                priority (Ordinary _) = 1
                priority (Capture ps) = 1 + length ps

        allowedMove a@(V2 ax ay) = ax `elem` [0..7] && ay `elem` [0..7] && isNothing (M.lookup a b)
        
        queenMoves p = map (+p) $ concat [[V2 d d, V2 d (-d)] | d <- [-7..7], d /= 0]
        wpawnMoves p = map (+p) [V2   1    1 , V2 (-1)  1 ]
        bpawnMoves p = map (+p) [V2 (-1) (-1), V2   1 (-1)]
        pawnMoves  p = wpawnMoves p ++ bpawnMoves p

        fieldMoves p t = map Ordinary $ filter allowedMove $ case t of
          White      -> wpawnMoves p
          WhiteQueen -> queenMoves p
          Black      -> bpawnMoves p
          BlackQueen -> queenMoves p

        fieldCaptures p t = case t of
          White      -> captures p [Black, BlackQueen] pawnMoves
          WhiteQueen -> captures p [Black, BlackQueen] queenMoves
          Black      -> captures p [White, WhiteQueen] pawnMoves
          BlackQueen -> captures p [White, WhiteQueen] queenMoves

        captures p en mv = rec' b p []
          where f b0 p0 = filter allowedMove $ map (\a -> a + vdir p0 a) $ filter (isJust . mfilter (`elem` en) . flip M.lookup b0) $ mv p0
                rec' b0 p0 l = case f b0 p0 of
                  [] -> [Capture $ reverse l]
                  m  -> concatMap (\m0 -> rec' (movePawn b0 p0 False $ Capture [m0]) m0 (m0:l)) m

range :: (Enum a, Num a) => V2 a -> V2 a -> [V2 a]
range (V2 fx fy) (V2 tx ty) = zipWith V2 (r fx tx) (r fy ty)
  where r a b = [a, a + signum (b - a) .. b]

movePawn :: Board -> V2 Int -> Bool -> Move -> Board
movePawn board from promotion move = M.insert to insert $ foldr M.delete board seg
  where to = case move of
          Ordinary x -> x
          Capture xs -> last xs
        
        seg = case move of
          Ordinary x -> range from x
          Capture xs -> concat $ zipWith range (from : xs) xs

        insert = case board M.! from of
          White -> if promotion && to ^. _y == 7 then WhiteQueen else White
          Black -> if promotion && to ^. _y == 0 then BlackQueen else Black
          a     -> a
