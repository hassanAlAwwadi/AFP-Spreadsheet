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
un_sym_table = []

fParser :: P.ReadP (Formula Int)
fParser = P.skipSpaces *> P.choice
  [ Raw <$> read @Int <$> P.munch1 C.isDigit
  , Ref
      <$ P.char 'c' <*> tParser
      <* P.char 'r' <*> tParser
  , flip Ref
      <$ P.char 'r' <*> tParser
      <* P.char 'c' <*> tParser
  , Un
    <$> uParser
    <*> fParser
  , Op
    <$> bParser
    <*> fParser
    <*> fParser
  ] where
    tParser = P.choice
      [ Loc <$> read @Int <$> P.munch1 C.isDigit
      , Rel <$ P.char '$' <*> (read @Int <$> P.munch1 C.isDigit)
      ]
    bParser = P.skipSpaces *> P.choice (map (\(k, f) -> P.string k *> pure f) bin_sym_table)
    uParser = P.skipSpaces *> P.choice (map (\(k, f) -> P.string k *> pure f) un_sym_table )

parseFormula :: String -> Maybe (Formula Int)
parseFormula s =
  let
    res = P.readP_to_S fParser s
  in
    case res of
      []     -> Nothing
      _      -> Just $ fst $ minimumBy (comparing (length . snd)) res