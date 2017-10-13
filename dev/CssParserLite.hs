{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module CssParserLite where

import           Text.Parsec
import           Text.Parsec.Text

data Selecter
  = ClassSelecter String
  | IDSelecter String
  | ElementSelecter String
  deriving Show

data CssBlock
  = RuleSet [Selecter] [CssBlock]
  | Comment String
  | Rule String
  deriving Show

padded :: Parser a -> Parser a
padded c = spaces *> c <* spaces

comment :: Parser CssBlock
comment = padded (string "/*") *> manyTill anyChar (try $ padded (string "*/")) >>= return.Comment <?> "comment"

rule :: Parser CssBlock
rule = many1 (noneOf "{};") <* (char ';' <|> lookAhead (char '}')) >>= return.Rule <?> "rule"

selecters :: Parser [Selecter]
selecters = label `sepBy1` separaters <?> "selecter"
  where
    label = (classSelecter <|> idSelecter <|> elementSelecter) <* spaces
    classSelecter = ClassSelecter <$> (char '.' *> many1 (nameSymbol <|> letter <|> digit))
    idSelecter = IDSelecter <$> (char '#' *> many1 (nameSymbol <|> letter <|> digit))
    elementSelecter = ElementSelecter <$> many1 (symbol <|> nameSymbol <|> letter <|> digit)
    nameSymbol = oneOf "-_"
    symbol = oneOf "\\\"*:=[]()|!@%^"
    separaters = skipMany (space <|> oneOf "+~>,")

ruleset :: Parser CssBlock
ruleset = ruleset' <?> "ruleset"
  where
    ruleset' = do
      s <- padded selecters
      padded (char '{')
      rules <- manyTill (try ruleset <|> try rule <|> try comment) (try $ padded (char '}'))
      return $ RuleSet s rules

css :: Parser [CssBlock]
css = many (comment <|> ruleset) <* eof
