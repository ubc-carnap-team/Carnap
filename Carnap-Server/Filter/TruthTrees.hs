{-# LANGUAGE OverloadedStrings #-}
module Filter.TruthTrees (makeTruthTrees) where

import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text as T
import Util.Data
import Filter.Util (numof, contentOf, intoChunks)
import Prelude

makeTruthTrees :: Block -> Block
makeTruthTrees cb@(CodeBlock (_, classes, extra) contents)
    | "TruthTree" `elem` classes = Div ("", [], []) $ map (activate classes extra) $ intoChunks contents
    | otherwise = cb
makeTruthTrees x = x

activate :: [Text] -> [(Text, Text)] -> Text -> Block
activate cls _extra chunk
    | "Prop" `elem` cls = Div (problemElId, [], []) [
            RawBlock "html" (T.concat
                [ "<script>Rudolf.createTree('"
                , problemElId
                , "', '"
                , escape . contentOf $ chunk
                , "');</script>" ])
        ]
    | otherwise = RawBlock "html" "<div>No matching truth tree logic</div>"
    where escape = T.pack . sanitizeForJS . T.unpack
          problemElId = T.concat ["problem-", numof chunk]
