{-# LANGUAGE TypeData, TypeFamilies, AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Spreadsheet where
import Spreadsheet.Input
import Spreadsheet.Unit 

import Data.Map(Map)
import Data.List(foldl', (\\), delete)
import qualified Data.Map as M

-- Placeholders
type Arr   = Map (Int, Int) (Formula, Int)
type Graph = Map (Int, Int) [(Int, Int)] 

data Spreadsheet = S { table :: Arr, backward :: Graph, forward :: Graph } 
  deriving Show

handle :: Input -> Spreadsheet -> Spreadsheet 
handle i (S a b f) = let 
  b' = updateBackwardGraph i b
  f' = updateForwardGraph i b b' f -- ^, yes, we need a lot of extra info
  a'  = updateArr i a
  a'' = propogate i a' f 
  in S a' b' f'

updateArr   :: Input  -> Arr -> Arr 
updateArr (Cell (x,y) v) a = M.insert (x,y) (v, eval a (x,y) v) a

propogate :: Input -> Arr -> Graph -> Arr
propogate (Cell (x,y) _) a f = 
  let next = concat $ f M.!? (x,y)  
  in undefined

updateBackwardGraph :: Input -> Graph -> Graph 
updateBackwardGraph (Cell (x,y) v) g = case v of 
  Raw _           -> M.insert (x,y) [] g
  Reference x' y' -> M.insert (x,y) [(locate x x', locate y y')] g
  Plus l r        -> let 
    start = M.insert (x,y) [] g 
    in adjustGraph r $ adjustGraph l start  where 
      adjustGraph (Raw _) graph           = graph
      adjustGraph (Plus l' r') graph      = adjustGraph r'  $ adjustGraph l' graph
      adjustGraph (Reference x' y') graph = M.alter 
        (\case Nothing  -> Just ([(locate x x', locate y y')])
               Just ls  -> Just ((locate x x', locate y y') : ls)) 
        (x,y) 
        graph 

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

altFold' :: Foldable t => b -> t a -> (b -> a -> b) -> b
altFold' l r f = foldl' f l r 
altAlter :: Ord k => k -> Map k a -> (Maybe a -> Maybe a) -> Map k a
altAlter r m f = M.alter f r m

eval :: Arr -> (Int, Int) -> Formula -> Int
eval _ _ (Raw i)                  = i
eval arr (x,y) (Reference x' y')  = let 
  (nx,ny) = (locate x x', locate y y')
  (_, r) = arr M.! (nx, ny) -- ^ otherwise malformed. Maybe want to add maybe to the end result
  in r
eval arr (x,y) (Plus l r) = let 
  l' = eval arr (x,y) l
  r' = eval arr (x,y) r 
  in l' + r' 

locate :: Int -> Ref -> Int 
locate _ (Loc i)           = i 
locate (lodestone) (Rel i) = lodestone + i

