module Formula where 
import Text.Show.Functions ()

data Formula a where
  Raw :: a -> Formula a 
  Ref :: Target -> Target -> Formula a
  Op  :: (a -> a -> a) -> Formula a -> Formula a -> Formula a
  Un  :: (a -> a) -> Formula a -> Formula a
  deriving Show

data Target 
  = Loc Int 
  | Rel Int
  deriving Show 

-- some helpers
direct :: Int -> Int -> Formula a 
direct x y = Ref (Loc x) (Loc y)

relative :: Int -> Int -> Formula a 
relative x y = Ref (Rel x) (Rel y)

locate :: Int -> Target -> Int 
locate _         (Loc i)   = i 
locate lodestone (Rel i)   = lodestone + i