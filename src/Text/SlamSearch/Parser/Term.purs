module Text.SlamSearch.Parser.Terms where

import Text.SlamSearch.Parser.Values
import Text.SlamSearch.Parser.Tokens

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Token
import Control.Apply
import Control.Alt
import Control.Alternative
import Data.Tuple
import Data.Either

data Predicate =
  ContainsPredicate Value
  | EqPredicate Value
  | GtPredicate Value
  | GtePredicate Value
  | LtePredicate Value
  | LtPredicate Value
  | NePredicate Value
  | LikePredicate Value

instance showPredicate :: Show Predicate where
  show a = case a of
    ContainsPredicate p -> "ContainsPredicate(" <> show p <> ")"
    EqPredicate p -> "EqPredicate(" <> show p <> ")"
    GtPredicate p -> "GtPredicate(" <> show p <> ")"
    GtePredicate p -> "GtePredicate(" <> show p <> ")"
    LtePredicate p -> "GtePredicate(" <> show p <> ")"
    LtPredicate p -> "LtPredicate(" <> show p <> ")"
    NePredicate p -> "NePredicate(" <> show p <> ")"
    LikePredicate p -> "LikePredicate(" <> show p <> ")"


instance predicateEq :: Eq Predicate where
  (==) (ContainsPredicate p) (ContainsPredicate p') = p == p'
  (==) (EqPredicate p) (EqPredicate p') = p == p'
  (==) (GtPredicate p) (GtPredicate p') = p == p'
  (==) (GtePredicate p) (GtePredicate p') = p == p'
  (==) (LtePredicate p) (LtePredicate p') = p == p'
  (==) (LtPredicate p) (LtPredicate p') = p == p'
  (==) (NePredicate p) (NePredicate p') = p == p'
  (==) (LikePredicate p) (LikePredicate p') = p == p'
  (==) _ _ = false
  (/=) a b = not $ a == b

data Label =
  Common String
  | Meta String

instance showLabel :: Show Label where
  show a = case a of
    Common p -> "Common(" <> show p <> ")"
    Meta p -> "Meta(" <> show p <> ")"

instance eqLabel :: Eq Label where
  (==) (Common p) (Common p') = p == p'
  (==) (Meta p) (Meta p') = p == p'
  (==) _ _ = false
  (/=) a b = not $ a == b

data SearchTermSimple = SearchTermSimple [Label] Predicate

instance showSearchTermSimpleEq :: Show SearchTermSimple where
  show (SearchTermSimple ls p) =
    "SearchTermSimple(" <> show ls <> "," <> show p <> ")"


instance eqSearchTermSimple :: Eq SearchTermSimple where
  (==) (SearchTermSimple ls p) (SearchTermSimple ls' p') =
    p == p' && ls == ls'
  (==) _ _ = false
  (/=) a b = not $ a == b

data SearchTerm =
  IncludeTerm SearchTermSimple
  | ExcludeTerm SearchTermSimple

instance searchTermEq :: Eq SearchTerm where
  (==) (IncludeTerm t) (IncludeTerm t') = t == t'
  (==) (ExcludeTerm t) (ExcludeTerm t') = t == t'
  (==) _ _  = false
  (/=) a b = not $ a == b

instance showSearchTerm :: Show SearchTerm where
  show a = case a of
    IncludeTerm t -> "Include(" <> show t <> ")"
    ExcludeTerm t -> "Exclude(" <> show t <> ")"

data PredicateAndLabel =
  P Predicate
  | L Label
  | I
  | E

instance showPredicateAndLabel :: Show PredicateAndLabel where
  show pl = case pl of
    P sp -> "P(" <> show sp <> ")"
    L l -> "L(" <> show l <> ")"
    I -> "I"
    E -> "E"


isP :: PredicateAndLabel -> Boolean
isP (P _) = true
isP _ = false

isL :: PredicateAndLabel -> Boolean
isL (L _) = true
isL _ = false

isI :: PredicateAndLabel -> Boolean
isI I = true
isI _ = false

isE :: PredicateAndLabel -> Boolean
isE E = true
isE _ = false 

i :: Parser [Value] PredicateAndLabel
i = get (Through Plus) *> pure I

e :: Parser [Value] PredicateAndLabel
e = get (Through Minus) *> pure E

l :: Parser [Value] PredicateAndLabel
l = do
  (try (when isLabel) >>= \(Label t) -> return $ L (Common t)) <|>
  (when isMeta >>= \(MetaLabel t) -> return $ L (Meta t))


containsPredicate :: Parser [Value] Predicate
containsPredicate = ContainsPredicate <$> when isTextual

eqPredicate :: Parser [Value] Predicate
eqPredicate =  get (Through Eq) *> (EqPredicate <$> when isTextual) 

gtPredicate :: Parser [Value] Predicate
gtPredicate =  get (Through Gt) *> (GtPredicate <$> when isTextual) 

gtePredicate :: Parser [Value] Predicate
gtePredicate = get (Through GtE) *> (GtePredicate <$> when isTextual) 

ltPredicate :: Parser [Value] Predicate
ltPredicate = get (Through Lt) *> (LtPredicate <$> when isTextual) 

ltePredicate :: Parser [Value] Predicate
ltePredicate = get (Through LtE) *> (LtePredicate <$> when isTextual) 

nePredicate :: Parser [Value] Predicate
nePredicate = get (Through Ne) *> (NePredicate <$> when isTextual) 

likePredicate :: Parser [Value] Predicate
likePredicate = get (Through Tilde) *> (LikePredicate <$> when isTextual) 
  
p :: Parser [Value] PredicateAndLabel
p = P <$> choice [try likePredicate,
                  try nePredicate,
                  try ltePredicate,
                  try ltPredicate,
                  try gtPredicate,
                  try gtePredicate,
                  try eqPredicate,
                  containsPredicate]

predicatesAndLabels :: Parser [Value] [PredicateAndLabel]
predicatesAndLabels = many $ choice [try p, try l, i, e]

getPredicate :: Parser [PredicateAndLabel] Predicate
getPredicate = do
  (P p) <- when isP
  return p

simpleTerm :: Parser [PredicateAndLabel] SearchTermSimple
simpleTerm = do
  ls <- try $ many (when isL >>= \(L l) -> return l)
  p <- getPredicate
  return $ SearchTermSimple ls p

searchTermI :: Parser [PredicateAndLabel] SearchTerm
searchTermI = do
  i <- option I (when isI)
  term <- simpleTerm
  return $ IncludeTerm term

searchTermE :: Parser [PredicateAndLabel] SearchTerm
searchTermE = do
  e <- when isE
  term <- simpleTerm
  return $ ExcludeTerm term

searchQuery :: Parser [PredicateAndLabel] [SearchTerm]
searchQuery = many $ choice [searchTermE, searchTermI]

search :: [Value] -> Either ParseError [SearchTerm]
search tokens =
  let parse = flip runParser in
  runParser tokens predicatesAndLabels >>= parse searchQuery

