{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                       hiding (div, head, id, span)

import           Control.Lens                  (view, (^.))
import           Control.Monad                 ((<=<))
import           Control.Monad.IO.Class        (liftIO)
import           Data.Extensible
import           Data.List                     (sortOn)
import           Data.Maybe                    (listToMaybe)
import           Data.String                   (IsString, fromString)
import           Data.Text                     (Text, unpack)
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy.IO             as TL
import           Data.Time
import qualified Data.Yaml                     as Y
import           ScrapBook                     (collect, fetch, toSite, write)
import qualified ScrapBook
import           System.Directory              (createDirectoryIfMissing)
import           System.Environment            (getArgs)
import           System.FilePath               (dropFileName)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              hiding (main)
import           Text.Blaze.Html5.Attributes   (class_, href, id, lang, rel,
                                                type_)

main :: IO ()
main = (listToMaybe <$> getArgs) >>= \case
  Nothing   -> error "please input config file path."
  Just path -> generate path =<< readConfig path

readConfig :: FilePath -> IO ScrapBook.Config
readConfig = either (error . show) pure <=< decodeFileEither'
  where
    decodeFileEither' path =
      fmap (ScrapBook.updateFileName feed' path) <$> Y.decodeFileEither path

generate :: FilePath -> ScrapBook.Config -> IO ()
generate path config = either (error . show) pure <=< collect $ do
  let sites = fmap toSite $ config ^. #sites
  posts <- concat <$> mapM fetch sites

  writeFeed (dropFileName path ++ name) =<< write config feed' posts

  writeHtml "./index.html" $ do
    div ! class_ "tabnav" $
      nav ! class_ "tabnav-tabs" $ do
        a ! href "/" ! class_ "tabnav-tab selected" $ "Posts"
        a ! href "/sites" ! class_ "tabnav-tab" $ "Authors"
    ul ! class_ "list-style-none" $ mapM_ postToHtml $
      take 50 $ reverse $ sortOn (view #date) posts

  writeHtml "./sites.html" $ do
    div ! class_ "tabnav" $
      nav ! class_ "tabnav-tabs" $ do
        a ! href "/" ! class_ "tabnav-tab" $ "Posts"
        a ! href "/sites" ! class_ "tabnav-tab selected" $ "Sites"
    ul ! class_ "list-style-none" $ mapM_ siteToHtml $ sortOn (view #title) sites

  where
    name = ScrapBook.fileName config feed'

writeFeed :: FilePath -> Text -> ScrapBook.Collecter ()
writeFeed path txt = liftIO $ writeFileWithDir path txt

writeHtml :: FilePath -> Html -> ScrapBook.Collecter ()
writeHtml path bodyHtml = liftIO . TL.writeFile path . renderHtml $
  docTypeHtml ! lang "jp" $ do
    head $ do
      title "Planet Haskell (JP)"
      link ! rel "stylesheet" ! type_ "text/css" !
        href "https://cdnjs.cloudflare.com/ajax/libs/Primer/10.0.0-rc.21/build.css"
      link ! rel "stylesheet" ! type_ "text/css" !
        href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
    body $ div ! class_ "container-md" $ do
      h1 ! id "header" $ do
        "Planet Haskell (JP)"
        a ! href "/feed" ! class_ "float-right link-gray-dark" $
          i ! class_ "fa fa-rss-square" $ ""
      bodyHtml

postToHtml :: ScrapBook.Post -> Html
postToHtml post = li ! class_ "border-bottom" $ do
  h3 $ a ! href (fromText $ post ^. #url) ! class_ "link-gray-dark" $
    toHtml (post ^. #title)
  p $ do
    let site = post ^. #site
    toHtml $ mconcat ["by ", site ^. #author]
    " on "
    span $ a ! href (fromText $ site ^. #url) ! class_ "link-gray-dark" $
      toHtml (site ^. #title)
    toHtml $ mconcat [" at ", formatTimeToDate $ unpack (post ^. #date)]

siteToHtml :: ScrapBook.Site -> Html
siteToHtml site = li ! class_ "border-bottom" $ do
  h3 $ a ! href (fromText $ site ^. #url) ! class_ "link-gray-dark" $
    toHtml (site ^. #title)
  p $ toHtml $ mconcat ["by ", site ^. #author]

feed' :: ScrapBook.Format
feed' = embedAssoc $ #feed @= ()

writeFileWithDir :: FilePath -> Text -> IO ()
writeFileWithDir path txt = do
  createDirectoryIfMissing True $ dropFileName path
  T.writeFile path txt

fromText :: IsString a => Text -> a
fromText = fromString . unpack

formatTimeToDate :: String -> String
formatTimeToDate =
  maybe "" (formatTime defaultTimeLocale "%B %d, %Y") . formatTimeFromRFC3339

-- take 10 == take (length "2018-02-02")
formatTimeFromRFC3339 :: String -> Maybe UTCTime
formatTimeFromRFC3339
  = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . take 10
