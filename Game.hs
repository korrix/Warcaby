{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Game where

import           Prelude hiding (lookup)
import qualified Data.Map.Strict as M

import Data.IORef
import Data.Maybe
import Control.Monad
import Linear.V2

import Types

lookup :: Ord k => M.Map k v -> k -> Maybe v
lookup = flip M.lookup

initialGameState :: GameState
initialGameState = GameState (M.fromList $ whites ++ blacks) Nothing Whites
  where whites = map (,White) [ V2 1 0, V2 3 0, V2 5 0, V2 7 0
                              , V2 0 1, V2 2 1, V2 4 1, V2 6 1
                              , V2 1 2, V2 3 2, V2 5 2, V2 7 2 ]

        blacks = map (,Black) [ V2 0 5, V2 2 5, V2 4 5, V2 6 5
                              , V2 1 6, V2 3 6, V2 5 6, V2 7 6
                              , V2 0 7, V2 2 7, V2 4 7, V2 6 7 ]
  --where whites = [(V2 4 3, WhiteQueen), (V2 4 7, WhiteQueen)]
  --      blacks = map (,Black) [V2 2 5, V2 6 1, V2 5 6]

updateGameState :: IORef GameState -> V2 Int -> IO ()
updateGameState gameStateRef p = do
  GameState brd sf trn <- readIORef gameStateRef
  let pm = possibleMoves trn brd
      save b s t = writeIORef gameStateRef $ GameState b s t

  case sf >>= lookup pm >>= findMove p of
    Just m -> save (movePawn brd (fromJust sf) m) Nothing (nextTurn trn)
    _      -> save brd (Just p) trn

possibleMoves :: Turn -> Board -> PossibleMoves
possibleMoves trn brd = prioritize $ M.mapWithKey allFieldMoves $ turnFilter trn brd
  where allFieldMoves p t = fieldMoves p t ++ fieldCaptures p t

        prioritize moves = M.filter (not . null) 
                         $ M.map (filter $ \x -> priority x >= maxPriority) moves
          where maxPriority = maximum' $ M.map (maximum' . map priority) moves
                maximum' l = if null l then 0 else maximum l
                priority (Ordinary _) = 1
                priority (Capture []) = -1
                priority (Capture ps) = 1 + length ps

        allowedMove a@(V2 ax ay) = ax `elem` [0..7] && ay `elem` [0..7] && isNothing (M.lookup a brd)
        
        allDirections = [V2 1 1, V2 (-1) 1, V2 (-1) (-1), V2 1 (-1)]

        fieldMoves p t = map Ordinary $ filter allowedMove $ uncurry (directional p brd takeWhile) $ case t of
          White      -> ([V2 1 1, V2 (-1) 1]      , 1)
          WhiteQueen -> (allDirections            , 6)
          Black      -> ([V2 (-1) (-1), V2 1 (-1)], 1)
          BlackQueen -> (allDirections            , 6)

        fieldCaptures p t = case t of
          White      -> captures p [Black, BlackQueen] 1
          WhiteQueen -> captures p [Black, BlackQueen] 6
          Black      -> captures p [White, WhiteQueen] 1
          BlackQueen -> captures p [White, WhiteQueen] 6

        directional p0 b0 f dirs step = concatMap d dirs
          where d dir = f (`M.notMember` b0) $ (p0+) . (dir*) . pure <$> [1..step]

        captures p en off = rec' brd p []
          where f b0 p0 = filter allowedMove
                        $ concatMap (\a -> directional a b0 takeWhile [vdir p0 a] off)
                        $ filter (isJust . mfilter (`elem` en) . lookup b0) 
                        $ directional p0 b0 takeWhile1 allDirections off

                rec' b0 p0 l = case f b0 p0 of
                  [] -> [Capture $ reverse l] 
                  m  -> concatMap (\m0 -> rec' (movePawn b0 p0 $ Capture [m0]) m0 (m0:l)) m

range :: (Enum a, Num a) => V2 a -> V2 a -> [V2 a]
range (V2 fx fy) (V2 tx ty) = zipWith V2 (r fx tx) (r fy ty)
  where r a b = [a, a + signum (b - a) .. b]

movePawn :: Board -> V2 Int -> Move -> Board
movePawn brd from move = M.insert to insert $ foldr M.delete brd seg
  where to@(V2 _ toY) = case move of
          Ordinary x -> x
          Capture xs -> last xs
        
        seg = case move of
          Ordinary x -> range from x
          Capture xs -> concat $ zipWith range (from : xs) xs

        insert = case brd M.! from of
          White -> if toY == 7 then WhiteQueen else White
          Black -> if toY == 0 then BlackQueen else Black
          a     -> a

gameScore :: GameState -> String
gameScore (GameState brd _ _) = w ++ " : " ++ b
  where w = show $ getScore Whites brd 
        b = show $ getScore Blacks brd
