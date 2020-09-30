{-# LANGUAGE OverloadedStrings #-}
module Filter.PrettyProof (makePrettyProof) where

import Text.Pandoc
import Prelude

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

makePrettyProof :: Block -> Block
makePrettyProof cb@(CodeBlock (_, classes, _extra) contents)
    | "PrettyProof" `elem` classes = BlockQuote [LineBlock $ wrap (T.lines contents)]
    | otherwise = cb
makePrettyProof x = x

wrap :: [Text] -> [[Inline]]
wrap [] = []
wrap [line] = [Span ("", ["conclusion"], []) (wrapLatex line)] : []
wrap (line : rest) = [Span ("", ["premise"], []) (wrapLatex line)] : wrap rest

wrapLatex :: Text -> [Inline]
wrapLatex line = intersperse Space $ intersperse (Str "&") $ map (Math InlineMath) (T.splitOn "&" line)
