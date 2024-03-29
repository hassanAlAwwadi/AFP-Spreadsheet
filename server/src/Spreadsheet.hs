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

cycleCheck :: Input -> Spreadsheet -> Maybe Spreadsheet
cycleCheck inp@(Cell c _) ss@(S _ b _) = if dfsDetect c (updateBackwardGraph inp b) 
                                         then Nothing 
                                         else Just ss

handle :: Input -> Spreadsheet -> Spreadsheet 
handle i (S a b f) = let 
  b'  = updateBackwardGraph i b
  f'  = updateForwardGraph i b b' f -- ^, yes, we need a lot of extra info
  a'  = updateArr i a
  a'' = propogate i a a' f' 
  in S a'' b' f'

updateArr   :: Input  -> Arr -> Arr 
updateArr (Cell (x,y) v) a = M.insert (x,y) (v, eval a (x,y) v) a

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

propogate :: Input -> Arr -> Arr -> Graph -> Arr
propogate (Cell (x,y) _) a a' f = let 
  v  = snd <$> (a  M.!? (x,y))
  v' = snd <$> (a' M.!? (x,y))
  nexts = topSort f -- there's probably a clever way so that every change only propagates
  -- through the relevant nodes in a topologically sorted order, but it's hard to think of that
  -- for now, propagate changes in the right order through the entire graph
  -- order is important for correctness
  -- nexts = bfPaths (\l -> concat $ f M.!? l) (x,y) -- need to make this a topological sort for everything to work out. 
  in if v == v'
    then a' 
    --else altFold' a' nexts $ \acc frontier -> altFold' acc frontier $ \acc' n -> altAlter n acc' $ \case 
    else altFold' a' nexts $ \acc n -> altAlter n acc $ \case
      Nothing      -> error "broken graph in propagation?"
      Just (fm, w) -> Just (fm, eval acc n fm) -- actually need to somehow interweave this with the bfs 
        -- because if w equals "eval acc' n fm", then we don't need to add its forward arcs into the frontier.
        -- ^not sure if this comment is still relevant (Review this Hassan?)

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

-- Notes:
-- For better or for worse, decided to stick with the Map representation of Graphs
-- Data.Graph sucks at repeated modifications of graphs 
-- Alga sucks at constructing the graph, although modification is very easy and has nice functions
-- Functional Graphs didn't have a topological sort

-- TODOS:
-- Need to integrate topSort in propogate