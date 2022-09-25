-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
