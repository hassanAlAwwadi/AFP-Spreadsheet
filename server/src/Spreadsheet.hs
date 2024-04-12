{-# LANGUAGE LambdaCase #-}
module Spreadsheet where
import Util
import Formula 
import Data.Map(Map)
import Spreadsheet.Input
import qualified Data.Map as M
import Data.List ((\\), delete, nub)

-- Placeholders
type Arr   = Map (Int, Int) (Formula Int, Int)
type Graph = Map (Int, Int) [(Int, Int)] 

data Spreadsheet = S { table :: Arr, backward :: Graph, forward :: Graph } 
  deriving Show

type Diff = [((Int,Int), Int)]
empty :: Spreadsheet
empty = S M.empty M.empty M.empty

cycleCheck :: Input -> Spreadsheet -> Maybe Spreadsheet
cycleCheck inp@(Cell c _) ss@(S _ b _) = if dfsDetect c (updateBackwardGraph inp b) 
                                         then Nothing 
                                         else Just ss

handle :: Input -> Spreadsheet -> (Spreadsheet, Diff)
handle i (S a b f) = let 
  b'           = updateBackwardGraph i b
  f'           = updateForwardGraph i b b' f -- ^, yes, we need a lot of extra info
  (a', diff)   = updateArr i a
  (a'', diff') = propogate i a (a', diff) f' 
  in (S a'' b' f', diff')

updateArr   :: Input  -> Arr -> (Arr, Diff) 
updateArr (Cell (x,y) v) a = let 
  value = eval a (x,y) v 
  in (M.insert (x,y) (v, value) a, [((x,y), value)])

updateBackwardGraph :: Input -> Graph -> Graph 
updateBackwardGraph (Cell (x,y) v) g = case v of 
  Raw _           -> M.delete (x,y) g
  Ref x' y'       -> M.insert (x,y) [(locate x x', locate y y')] g
  Op _ l r        -> let 
    start = M.insert (x,y) [] g 
    in M.alter (nub <$>) (x,y) $ adjustGraph r $ adjustGraph l start   where 
      adjustGraph (Raw _) graph           = graph
      adjustGraph (Op _ l' r') graph      = adjustGraph r'  $ adjustGraph l' graph
      adjustGraph (Ref x' y') graph = altAlter (x,y) graph $ \case 
        Nothing  -> Just ([(locate x x', locate y y')])
        Just ls  -> Just ((locate x x', locate y y') : ls)
        

updateForwardGraph :: Input -> Graph -> Graph -> Graph -> Graph 
updateForwardGraph (Cell (x,y) _) b b' f = let 
  old_edges = concat $ b M.!? (x,y)
  new_edges = concat $ b' M.!? (x,y) 
  rem_edges = old_edges \\ new_edges
  add_edges = new_edges \\ old_edges
  f'  = altFold' f rem_edges $ \acc r -> altAlter r acc $ \case 
      Nothing -> Nothing     
      Just l  -> let l' = delete (x,y) l in if null l' then Nothing else Just l'
  f'' = altFold' f' add_edges $ \acc n -> altAlter n acc $ \case 
    Nothing -> Just [(x,y)] 
    Just l  -> Just $ (x,y):l   
  in f'' 

propogate :: Input -> Arr -> (Arr, Diff) -> Graph -> (Arr, Diff)
propogate (Cell (x,y) _) a (a', diff) f = let 
  v  = snd <$> (a  M.!? (x,y))
  v' = snd <$> (a' M.!? (x,y))
  nexts = topSort f 
  in if v == v'
    then (a', diff) 
    else altFold' (a', diff) nexts $ \(acc, diff) n -> case acc M.!? n of
      Nothing      -> error "broken graph in propagation?"
      Just (fm, w) -> let 
        value = eval acc n fm 
        in (M.insert n (fm, value) acc, (n, value):diff)

eval :: Arr -> (Int, Int) -> Formula Int -> Int
eval _   _     (Raw i)      = i
eval arr (x,y) (Ref x' y')  = let 
  (nx,ny) = (locate x x', locate y y')
  (_, r) = arr M.! (nx, ny) -- ^ otherwise malformed. Maybe want to add Maybe to the end result
  in r
eval arr (x,y) (Op f l r) = let
  l' = eval arr (x,y) l
  r' = eval arr (x,y) r 
  in f l' r' 
eval arr (x,y) (Un f z) = let
  z' = eval arr (x,y) z
  in f z'