{-
  This is a small program to solve the Towers of Hanoi puzzle for an arbitrary
  number of disks and pegs. It simply uses the A* search algorithm to find an
  optimal path in the search space of valid puzzle states.

  Author: Pieter Agten <pieter.agten@gmail.com>
  Date: 4 March 2015
-}

import Data.Graph.AStar
import Data.Set
import Data.List
import Text.Printf
import System.Environment
import System.Console.GetOpt

type Hanoi = [[Int]]

nextStates :: Hanoi -> Set Hanoi
nextStates h = Data.Set.fromList([move h from to | from <- [0..(length h)-1], to <- [0..(length h)-1], from /= to, validMove h from to])

validMove :: Hanoi -> Int -> Int -> Bool
validMove h from to = 
  let fromList = h!!from in
  let toList = h!!to in
  (not (Data.List.null(h !! from))) && (Data.List.null(toList) || head(fromList) < head(toList))  

move :: Hanoi -> Int -> Int -> Hanoi
move h from to =
  let (h0,val) = takeTop h from in
  putOnTop h0 val to

takeTop :: Hanoi -> Int -> (Hanoi,Int)
takeTop ((top:rest):tl) 0 = (rest:tl, top)
takeTop (hd:tl) i = 
  let (h0,val) = takeTop tl (i-1) in
  (hd:h0,val)

putOnTop :: Hanoi -> Int -> Int -> Hanoi
putOnTop (hd:tl) val 0 = (val:hd):tl
putOnTop (hd:tl) val pos = hd:(putOnTop tl val (pos-1))

distance :: Hanoi -> Hanoi -> Int
distance _ _ = 1

heuristic :: Hanoi -> Hanoi -> Int
heuristic goal h = sum (zipWith (\ h0 h1 -> abs ((length h0) - (length h1))) goal h)

findPath :: Hanoi -> Hanoi -> Maybe [Hanoi]
findPath startState goalState = Data.Graph.AStar.aStar nextStates distance (heuristic goalState) (== goalState) startState

makeState :: Int -> Int -> Int -> Hanoi
makeState nbDisks nbPegs peg = [(if p == peg then [0..nbDisks-1] else []) | p <- [0..nbPegs-1]]

printHanoiList h = mapM_ print h

main = do
  args <- getArgs
  progName <- getProgName
  case getOpt RequireOrder [] args of
    (_, [nbDisks, nbPegs, startPeg, endPeg], _) ->
      let nbDisks' = read nbDisks :: Int;
          nbPegs' = read nbPegs :: Int;
          startPeg' = read startPeg :: Int;
          endPeg' = read endPeg :: Int;
          startState = makeState nbDisks' nbPegs' startPeg';
          goalState = makeState nbDisks' nbPegs' endPeg' in
            case findPath startState goalState of
              Nothing -> print "No path found!"
              Just p -> do {
                printf "Number of steps: %d\n" (length p);
                print startState;
                printHanoiList p
              }
    (_, _, _) -> printf "Usage: %s <nb-of-disks> <nb-of-pegs> <start-peg> <end-peg>\n" progName

