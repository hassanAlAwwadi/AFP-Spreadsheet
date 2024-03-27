-- | a module that exists because putting the function at the end of the arugment list 
--   makes aligning stuff nicer. 
module Util where
import Data.Map(Map, alter)
import Data.List(foldl')
import Control.Monad.IO.Class(MonadIO)
import qualified Text.Pretty.Simple as T
import Control.Monad.State.Strict
import qualified Data.Set as S
import qualified Data.Map as M

altFold' :: Foldable t => b -> t a -> (b -> a -> b) -> b
altFold' l r f = foldl' f l r

altAlter :: Ord k => k -> Map k a -> (Maybe a -> Maybe a) -> Map k a
altAlter r m f = alter f r m

bfPaths :: (a -> [a]) -> a -> [[a]]
bfPaths step seed  =  [seed] : go (step seed) where
  go [] = []
  go xs = xs : go (concatMap step xs)

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = T.pPrintOpt T.CheckColorTty ( T.defaultOutputOptionsDarkBg { T.outputOptionsCompact = True } )

dfsDetect :: (Int, Int) -> Map (Int, Int) [(Int, Int)] -> Bool
dfsDetect i g = not $ evalState (dfs' i g) S.empty

dfs' :: (Int, Int) -> Map (Int, Int) [(Int, Int)] -> State (S.Set (Int, Int)) Bool
dfs' i g = do
  visited <- get
  if S.member i visited
  then return False
  else do
    let restBools = map (\x -> evalState (dfs' x g) (S.insert i visited))
                        (M.findWithDefault [] i g)
    if any not restBools then return False else return True

topSort :: Map (Int, Int) [(Int, Int)] -> [(Int, Int)]
topSort g = evalState (topSort' g (M.keys g)) M.empty

data Mark = Perm | Temp deriving Eq
type MarkedStatus = Map (Int, Int) Mark

topSort' :: Map (Int, Int) [(Int, Int)] -> [(Int, Int)] -> State MarkedStatus [(Int, Int)]
topSort' _ []     = return []
topSort' g (n:ns) = do
  ms <- get
  if M.findWithDefault Temp n ms == Perm
  then topSort' g ns
  else do
    visit g n
    put (M.insert n Perm ms)
    restRes <- topSort' g ns
    return (n : restRes)

visit :: Map (Int, Int) [(Int, Int)] -> (Int, Int) -> State MarkedStatus ()
visit g currNode = do
  ms <- get
  if M.findWithDefault Perm currNode ms == Temp
  then error "Top Sort detected a cycle"
  else do
    put (M.insert currNode Temp ms)
    let neighbours = M.findWithDefault [] currNode g
    mapM_ (visit g) neighbours