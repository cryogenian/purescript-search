module Text.SlamSearch.Parser.Tokens
  ( tokens
  , Token(..)
  , isText
  , keyChars
  ) where

import Prelude

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Control.Apply ((<*))
import Control.Alt ((<|>))
import Data.Foldable (fold)
import Data.List (many, List(..), fromList)
import Data.Either (Either())
import Data.String (fromChar, toCharArray, fromCharArray)
import Data.Char (fromCharCode)

-- | Chars that can not be used in query strings without escape.
-- | This is invalid: `foo.bar`
-- | This is invalid: `"foo.bar"`
-- | This is invalid: `foo\.bar`
-- | This is valid: `"foo\.bar"`
keyChars ∷ Array Char
keyChars = toCharArray $ fold
  [ "."
  , "~"
  , "!"
  , "@"
  , "#"
  , "("
  , ")"
  , "-"
  , "+"
  , "="
  , "<"
  , ">"
  , " "
  , "\t"
  , "\r"
  , "\n"
  , "\""
  , ":"
  , ""
  ]

rawString ∷ P.Parser String String
rawString = do
  cs ← many $ PS.noneOf keyChars
  case cs of
    Nil → P.fail "incorrect raw string"
    _ → pure $ fromCharArray $ fromList cs

slashed ∷ P.Parser String String
slashed = do
  slash ← PS.string "\\"
  ch ← PS.anyChar
  pure $ fromChar ch

quotedSymbol ∷ P.Parser String String
quotedSymbol = do
  (PC.try slashed) <|> (fromChar <$> PS.noneOf [fromCharCode 34])

quotedString ∷ P.Parser String String
quotedString =
  PC.between (PS.string "\"") (PS.string "\"") do
    cs ← many quotedSymbol
    case cs of
      Nil → P.fail "incorrect quoted string"
      _ → pure $ fold cs

data Token
  = Text String
  | Range
  | Hash
  | Plus
  | Minus
  | At
  | Eq
  | Lt
  | Gt
  | LtE
  | GtE
  | Ne
  | Tilde
  | Colon

instance showToken ∷ Show Token where
  show =
    case _ of
      Text s → "Text(" <> s <> ")"
      Range → "Range"
      Hash → "Hash"
      Plus → "Plus"
      Minus → "Minus"
      At → "At"
      Eq → "Eq"
      Lt → "Lt"
      Gt → "Gt"
      LtE → "LtE"
      GtE → "GtE"
      Ne → "Ne"
      Tilde → "Tilde"
      Colon → "Colon"

instance eqToken ∷ Eq Token where
  eq (Text s) (Text s') = s == s'
  eq Range Range = true
  eq Hash Hash = true
  eq Plus Plus = true
  eq Minus Minus = true
  eq At At = true
  eq Eq Eq = true
  eq Lt Lt = true
  eq Gt Gt = true
  eq LtE LtE = true
  eq GtE GtE = true
  eq Ne Ne = true
  eq Tilde Tilde = true
  eq Colon Colon = true
  eq _ _ = false

-- | Check if token is wrapper for string, used in `Text.SlamSearch.P.Parser`
isText ∷ Token → Boolean
isText (Text _) = true
isText _ = false

raw ∷ P.Parser String Token
raw = Text <$> rawString

quoted ∷ P.Parser String Token
quoted = Text <$> quotedString

range ∷ P.Parser String Token
range = pure Range <* PS.string ".."

hash ∷ P.Parser String Token
hash = pure Hash <* PS.string "#"

plus ∷ P.Parser String Token
plus = pure Plus <* PS.string "+"

minus ∷ P.Parser String Token
minus = pure Minus <* PS.string "-"

at ∷ P.Parser String Token
at = pure At <* PS.string "@"

eq_ ∷ P.Parser String Token
eq_ = pure Eq <* PS.string "="

lt ∷ P.Parser String Token
lt = pure Lt <* PS.string "<"

gt ∷ P.Parser String Token
gt = pure Gt <* PS.string ">"

lte ∷ P.Parser String Token
lte = pure LtE <* PS.string "<="

gte ∷ P.Parser String Token
gte = pure GtE <* PS.string ">="

ne ∷ P.Parser String Token
ne = pure Ne <* (PS.string "!=" <|> PS.string "<>")

tilde ∷ P.Parser String Token
tilde = pure Tilde <* PS.string "~"

colon ∷ P.Parser String Token
colon = pure Colon <* PS.string ":"

tokenize ∷ P.Parser String (List Token)
tokenize =
  many $
    PC.choice
      [ colon, tilde, ne, gte, lte, gt
      , lt, eq_, at, minus, plus, hash
      , range, quoted, raw
      ]

-- | Parse `String` to list of `Token`s.
tokens ∷ String → Either P.ParseError (List Token)
tokens input = P.runParser input tokenize
