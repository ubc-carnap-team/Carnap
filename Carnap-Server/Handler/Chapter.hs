module Handler.Chapter where

import Import
import Yesod.Markdown
import Data.Char (isDigit)
import Filter.Sidenotes
import Util.Handler hiding (fileToHtml)
import Text.Pandoc
import Text.Pandoc.Walk (walkM, walk)
import System.Directory (getDirectoryContents)
import Text.Julius (juliusFile)
import Text.Hamlet (hamletFile)
import TH.RelativePaths (pathRelativeToCabalPackage)
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Control.Monad.State (evalState)

-- XXX Fair amount of code-duplication between this and Handler/Book.hs. Perhaps merge those modules.

getChapterR :: Int -> Handler Html
getChapterR n = do bookdir <- appBookRoot <$> (appSettings <$> getYesod)
                   cdir <- liftIO $ getDirectoryContents bookdir
                   content' <- liftIO $ content n cdir bookdir
                   case content' of
                       Right (Right html) -> chapterLayout
                            [whamlet|
                                <div.container>
                                    <article>
                                        #{html}
                                        <nav.nextAndPrev>
                                            <p>
                                                $if (n > 1)
                                                    <a href="@{ChapterR (n - 1)}">
                                                        Previous Chapter
                                                $if ((n > 1) && (n < (length cdir - 3)))
                                                    <span>∙
                                                $if (n < (length cdir - 3))
                                                    <a href="@{ChapterR (n + 1)}">
                                                        Next Chapter
                            |]

                       Right (Left err) -> defaultLayout
                                      [whamlet|
                                        <div.container>
                                            <article>
                                                #{show err}
                                       |]
                       Left err -> defaultLayout
                                      [whamlet|
                                        <div.container>
                                            <article>
                                                #{show err}
                                       |]

content :: Int -> [FilePath] -> FilePath -> IO (Either PandocError (Either PandocError Html))
content n cdir cdirp = do let matches = filter (\x -> (show n ++ ".pandoc") == dropWhile (not . isDigit) x) cdir
                          case matches of
                              [] -> do print ("no matches"::Text)
                                       fileToHtml cdirp ""
                              (m:_)  -> fileToHtml cdirp m

fileToHtml :: FilePath -> FilePath -> IO (Either PandocError (Either PandocError Html))
fileToHtml path m = do md <- markdownFromFile (path </> m)
                       case parseMarkdown yesodDefaultReaderOptions { readerExtensions = exts } md of
                           Right pd -> do let pd' = applyFilters pd
                                          return $ Right $ writePandocTrusted yesodDefaultWriterOptions { writerExtensions = exts } pd'
                           Left e -> return $ Left e
    where exts = extensionsFromList
                    [ Ext_raw_html
                    , Ext_markdown_in_html_blocks
                    , Ext_auto_identifiers
                    , Ext_tex_math_dollars
                    , Ext_fenced_code_blocks
                    , Ext_backtick_code_blocks
                    , Ext_line_blocks
                    , Ext_fancy_lists
                    , Ext_definition_lists
                    , Ext_example_lists
                    , Ext_simple_tables
                    , Ext_multiline_tables
                    , Ext_footnotes
                    , Ext_fenced_code_attributes
                    , Ext_yaml_metadata_block
                    ]

applyFilters= let walkNotes y = evalState (walkM makeSideNotes y) 0
                  walkProblems y = walk (allFilters) y
                  in walkNotes . walkProblems

chapterLayout :: ToWidget App a => a -> Handler Html
chapterLayout widget = do
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
        pc <- widgetToPageContent $ do
            toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/command.julius")
            toWidgetHead $(juliusFile =<< pathRelativeToCabalPackage "templates/status-warning.julius")
            toWidgetHead [julius|var submission_source="book";|]

            addScript $ StaticR js_popper_min_js
            addScript $ StaticR ghcjs_rts_js
            addScript $ StaticR ghcjs_allactions_lib_js
            addScript $ StaticR ghcjs_allactions_out_js
            addStylesheet $ StaticR css_tree_css
            addStylesheet $ StaticR css_tufte_css
            addStylesheet $ StaticR css_tuftextra_css
            addStylesheet $ StaticR css_exercises_css
            addStylesheet $ StaticR css_prettyproof_css
            $(widgetFile "default-layout")
            addScript $ StaticR ghcjs_allactions_runmain_js
            -- Scripts to insert Rudolf truth-tree widget
            addStylesheetRemote "https://unpkg.com/truth-tree/dist/lib.css"
            addScript $ StaticR js_createTrees_js
        withUrlRenderer $(hamletFile =<< pathRelativeToCabalPackage "templates/default-layout-wrapper.hamlet")
