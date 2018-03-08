{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import           Control.Lens           ((^.))
import           Control.Monad          ((<=<))
import           Control.Monad.IO.Class (liftIO)
import           Data.Extensible
import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text.IO           as T
import qualified Data.Yaml              as Y
import           ScrapBook              (collect, fetch, toSite, write)
import qualified ScrapBook              as ScrapBook
import           System.Directory       (createDirectoryIfMissing)
import           System.Environment     (getArgs)
import           System.FilePath        (dropFileName)

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
  posts <- concat <$> mapM (fetch . toSite) (config ^. #sites)
  writeFeed (dropFileName path ++ name) =<< write config feed' posts
  pure ()
  where
    name = ScrapBook.fileName config feed'

writeFeed :: FilePath -> Text -> ScrapBook.Collecter ()
writeFeed path txt = liftIO $ writeFileWithDir path txt

feed' :: ScrapBook.Format
feed' = embedAssoc $ #feed @= ()

writeFileWithDir :: FilePath -> Text -> IO ()
writeFileWithDir path txt = do
  createDirectoryIfMissing True $ dropFileName path
  T.writeFile path txt
