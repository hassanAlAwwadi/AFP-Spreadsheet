-- | a module that exists because putting the function at the end of the arugment list 
--   makes aligning stuff nicer. 
module Util where
import Data.Map(Map, alter)
import Data.List(foldl')
import Control.Monad.IO.Class(MonadIO)
import qualified Text.Pretty.Simple as T

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