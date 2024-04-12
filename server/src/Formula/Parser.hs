module Formula.Parser where

import Formula
import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Text.ParserCombinators.ReadP as P
import Data.List ( minimumBy )
import Data.Ord (comparing)

bin_sym_table :: [(String, (Int -> Int -> Int))]
bin_sym_table =  [("*", (*)), ("+", (+)), ("-", (-)), ("max", max), ("min", min)]

un_sym_table :: [(String, (Int -> Int))]
un_sym_table = [("(-)", negate)]

fParser :: P.ReadP (Formula Int)
fParser = P.skipSpaces *>
  (    P.char '(' *> fParser <* P.char ')'
  P.<++ Raw <$> read @Int <$> P.munch1 C.isDigit
  -- very fragile
  P.<++ Ref
      <$ P.char 'c' <*> tParserC
      <* P.char '-' 
      <* P.char 'r' <*> tParserR
  P.<++ flip Ref
      <$ P.char 'r' <*> tParserR
      <* P.char '-' 
      <* P.char 'c' <*> tParserC
  P.<++ Un
    <$> uParser
    <*> fParser
  P.<++ Op
    <$> bParser
    <*> fParser
    <*> fParser
  ) where
    tParserR = P.choice
      [ Loc <$> read @Int <$> P.munch1 C.isDigit
      , Rel <$ P.char '$' <*> (P.choice [id <$ P.char '+', (*(-1)) <$ P.char '-'] <*> (read @Int <$> P.munch1 C.isDigit))
      ]
    tParserC = P.choice
      [ Loc <$> base26tobase10 <$> P.munch1 C.isAlpha
      , Rel <$ P.char '$' <*> (P.choice [id <$ P.char '+', (*(-1)) <$ P.char '-'] <*> (read @Int <$> P.munch1 C.isDigit))
      ]
    base26tobase10 :: String -> Int 
    base26tobase10 = go 0 where 
      go n ['a'] = n -- special exception for a's at the last spot, where for some reason they represent a 0
      go n [] = n 
      go n (s:tr) = case base26 M.!? s  of 
        Just v  -> go (n*26 + v) tr
        Nothing -> n 


    bParser = P.skipSpaces *> P.choice (map (\(k, f) -> P.string k *> pure f) bin_sym_table)
    uParser = P.skipSpaces *> P.choice (map (\(k, f) -> P.string k *> pure f) un_sym_table )

-- | trying to make sure it only gets calculated once so expelling it from the where
base26 :: Map Char Int
base26 = M.fromList $ zip ['a' .. 'z'] [1 .. 26]

parseFormula :: String -> Maybe (Formula Int)
parseFormula s =
  let
    res  = P.readP_to_S fParser s
    mins = minimumBy (comparing (length . snd)) res
    sm   = snd mins
  in
    case res of
      []     -> Nothing
      _      -> if sm == "" then Just (fst mins) else Nothing