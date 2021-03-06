module Util.Handler where

import Import
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Yesod.Markdown
import Text.Pandoc (Block, MetaValue(..),Inline(..), writerExtensions,writerWrapText, WrapOption(..), readerExtensions, Pandoc(..))
import Text.Pandoc.Walk (walkM, walk)
import Text.Julius (juliusFile,rawJS)
import Text.Hamlet (hamletFile)
import TH.RelativePaths (pathRelativeToCabalPackage)
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

cleanLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        authmaybe <- maybeAuth
        (isInstructor, mdoc, mcourse) <- case authmaybe of
            Nothing -> return (False, Nothing, Nothing)
            Just uid -> runDB $ do
                mud <- getBy $ UniqueUserData $ entityKey uid
                mcour <- maybe (return Nothing) get (mud >>= userDataEnrolledIn . entityVal)
                masgn <- maybe (return Nothing) get (mcour >>= courseTextBook)
                mdoc <- maybe (return Nothing) get (assignmentMetadataDocument <$> masgn)
                return (not $ null (mud >>= userDataInstructorId . entityVal), mdoc, mcour)
        pc <- widgetToPageContent $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile =<< pathRelativeToCabalPackage "templates/default-layout-wrapper.hamlet")

-- * Pandoc
allFilters :: Block -> Block
allFilters = makeTreeDeduction
             . makeCounterModelers
             . makePrettyProof
             . makeProofChecker
             . makeQualitativeProblems
             . makeSequent
             . makeSynCheckers
             . makeTranslate
             . makeTreeDeduction
             . makeTruthTables
             . makeTruthTrees
             . renderFormulas

retrievePandocVal metaval = case metaval of
                        Just (MetaInlines ils) -> return $ Just (catMaybes (map fromStr ils))
                        Just (MetaList list) -> do mcsses <- mapM retrievePandocVal (map Just list)
                                                   return . Just . concat . catMaybes $ mcsses
                        Nothing -> return Nothing
                        x -> setMessage (toHtml ("bad yaml metadata: " ++ show x)) >> return Nothing
    where fromStr (Str x) = Just x
          fromStr _ = Nothing

fileToHtml filters path = do Markdown md <- markdownFromFile path
                             let md' = Markdown (filter ((/=) '\r') md) --remove carrage returns from dos files
                             case parseMarkdown yesodDefaultReaderOptions { readerExtensions = carnapPandocExtensions } md' of
                                 Right pd -> do let pd'@(Pandoc meta _)= walk filters pd
                                                return $ Right $ (write pd', meta)
                                 Left e -> return $ Left e
    where write = writePandocTrusted yesodDefaultWriterOptions { writerExtensions = carnapPandocExtensions, writerWrapText = WrapPreserve }

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
