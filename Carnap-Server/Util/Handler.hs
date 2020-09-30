module Util.Handler where

import Import
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Text.Pandoc (Block, MetaValue(..),Inline(..))
import Text.Julius (juliusFile,rawJS)
import Text.Hamlet (hamletFile)
import Util.Data
import Util.Database

import Filter.SynCheckers
import Filter.PrettyProof
import Filter.ProofCheckers
import Filter.Translate
import Filter.TruthTables
import Filter.TruthTrees
import Filter.CounterModelers
import Filter.Qualitative
import Filter.Sequent
import Filter.TreeDeduction
import Filter.RenderFormulas

minimalLayout c = [whamlet|
                  <div.container>
                      <article>
                          #{c}
                  |]

-- * Pandoc
allFilters :: Block -> Block
allFilters = makeTreeDeduction
             . makeCounterModelers
             . makeProofChecker
             . makeQualitativeProblems
             . makeSequent
             . makeSynCheckers
             . makeTranslate
             . makeTreeDeduction
             . makeTruthTables
             . makeTruthTrees
             . makePrettyProof
             . renderFormulas

retrievePandocVal metaval = case metaval of
                        Just (MetaInlines ils) -> return $ Just (catMaybes (map fromStr ils))
                        Just (MetaList list) -> do mcsses <- mapM retrievePandocVal (map Just list)
                                                   return . Just . concat . catMaybes $ mcsses
                        Nothing -> return Nothing
                        x -> setMessage (toHtml ("bad yaml metadata: " ++ show x)) >> return Nothing
    where fromStr (Str x) = Just x
          fromStr _ = Nothing

-- * Misc

serveDoc :: (Document -> FilePath -> Handler a) -> Document -> FilePath -> UserId -> Handler a
serveDoc sendIt doc path creatoruid = case documentScope doc of
                                Private -> do
                                  muid <- maybeAuthId
                                  case muid of Just uid' | uid' == creatoruid -> sendIt doc path
                                               _ -> notFound
                                _ -> sendIt doc path

asFile :: Document -> FilePath -> Handler TypedContent
asFile doc path = do addHeader "Content-Disposition" $ concat
                        [ "attachment;"
                        , "filename=\"", documentFilename doc, "\""
                        ]
                     sendFile typeOctet path

asCss :: Document -> FilePath -> Handler TypedContent
asCss _ path = sendFile typeCss path

asJs :: Document -> FilePath -> Handler TypedContent
asJs _ path = sendFile typeJavascript path
