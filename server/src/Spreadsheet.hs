{-# LANGUAGE TypeData, TypeFamilies, AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Spreadsheet where
import Spreadsheet.Input
import Spreadsheet.Unit 

import Data.Map(Map)
import Data.List(foldl')
import qualified Data.Map as M

-- Placeholders
type Arr   = Map (Int, Int) (Formula, Int)
type Graph = Map (Int, Int) [(Int, Int)]

data Spreadsheet = S Arr Graph 

handle :: Input -> Spreadsheet -> Spreadsheet 
handle i (S a g) = let 
  g' = updateGraph i g 
  a' = updateArr i a 
  in S a' g'

updateArr   :: Input  -> Arr -> Arr 
updateArr (Aether) a       = a
updateArr (Cell (x,y) v) a = M.insert (x,y) (v, eval a (x,y) v) a

updateGraph :: Input -> Graph -> Graph 
updateGraph Aether g = g 
updateGraph (Cell (x,y) v) g = case v of 
  Raw _           -> M.insert (x,y) [] g
  Reference x' y' -> M.insert (x,y) [(locate x x', locate y y')] g
  Plus l r        -> let 
    start = M.insert (x,y) [] g 
    in gentleUpdate r $ gentleUpdate l start  where 
      gentleUpdate (Raw _) graph           = graph
      gentleUpdate (Plus l' r') graph      = gentleUpdate r'  $ gentleUpdate l' graph
      gentleUpdate (Reference x' y') graph = M.alter 
        (\case Nothing  -> Just ([(locate x x', locate y y')])
               Just ls  -> Just ((locate x x', locate y y') : ls)) 
        (x,y) 
        graph 

eval :: Arr -> (Int, Int) -> Formula -> Int
eval _ _ (Raw i)                  = i
eval arr (x,y) (Reference x' y')  = let 
  (nx,ny) = (locate x x', locate y y')
  (_, r) = arr M.! (nx, ny) 
  in r
eval arr (x,y) (Plus l r) = let 
  l' = eval arr (x,y) l
  r' = eval arr (x,y) r 
  in l' + r' 

locate :: Int -> Ref -> Int 
locate _ (Loc i)           = i 
locate (lodestone) (Rel i) = lodestone + i