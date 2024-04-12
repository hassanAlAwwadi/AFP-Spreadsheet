{-# LANGUAGE ScopedTypeVariables #-}
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


run_parser :: String -> (Maybe (Formula Int))
run_parser str = case P.readP_to_S fParser str of 
  []     -> Nothing 
  (x:xs) -> Just (fst x)

fParser :: P.ReadP (Formula Int)
fParser = P.skipSpaces *>
  (     (P.char '(' *> fParser <* P.char ')')
  P.<++ (Raw <$> read @Int <$> P.munch1 C.isDigit)
  -- very fragile
  P.<++ (Ref
      <$ P.char 'c' <*> tParser
      <* P.choice [P.char '-', P.char ':', pure ' ']
      <* P.char 'r' <*> tParser)
  P.<++ (flip Ref
      <$ P.char 'r' <*> tParser
      <* P.choice [P.char '-', P.char ':', pure ' ']
      <* P.char 'c' <*> tParser)
  P.<++ (Un
    <$> uParser
    <*> fParser)
  P.<++ (Op
    <$> bParser
    <*> fParser
    <*> fParser)
  ) where
    tParser = P.choice
      [ Loc <$> read @Int <$> P.munch1 C.isDigit
      , Rel <$ P.char '$' <*> (P.choice [id <$ P.char '+', (*(-1)) <$ P.char '-'] <*> (read @Int <$> P.munch1 C.isDigit))
      ]

    bParser = P.skipSpaces *> P.choice (map (\(k, f) -> P.string k *> pure f) bin_sym_table)
    uParser = P.skipSpaces *> P.choice (map (\(k, f) -> P.string k *> pure f) un_sym_table )


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