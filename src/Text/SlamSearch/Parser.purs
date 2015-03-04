module Text.SlamSearch.Parser (
  parseSearchQuery
  ) where

import Data.Either
import Text.Parsing.Parser 
import Text.SlamSearch.Parser.Terms (search, SearchTerm(..))
import Text.SlamSearch.Parser.Tokens (tokens)
import Text.SlamSearch.Parser.Values (values)

parseSearchQuery :: String -> Either ParseError [SearchTerm]
parseSearchQuery input =
  pure input >>=
  tokens >>=
  values >>=
  search



