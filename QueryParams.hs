{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module QueryParams where

import Control.Monad.IO.Class
import qualified Data.Text         as T
import Data.Text (Text)
import Network.URI

import Reflex.Dom
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.Window as Window

parseQueryString :: Text -> [(Text, Text)]
parseQueryString = map parseKV . T.splitOn "&"
  where
    parseKV kv =
      case T.splitOn "=" kv of
        [k,v] -> (k, T.pack $ unEscapeString $ T.unpack v)
        [k]   -> (k, "")
        _     -> error "parseQueryString: invalid query string"

queryParamsEvent
  :: forall m t.
     ( Reflex t
     , MonadIO (Performable m)
     , PostBuild t m
     , PerformEvent t m
     ) => m (Event t [(Text, Text)])
queryParamsEvent = do
  onLoadEvent <- getPostBuild
  performEvent $ getQueryParams <$ onLoadEvent
    where
      getQueryParams :: Performable m [(Text, Text)]
      getQueryParams = do
        window <- DOM.currentWindowUnchecked
        location <- Window.getLocation window
        uri <- getLocationUri location
        let params = parseQueryString $ T.pack $ drop 1 $  uriQuery uri
        liftIO $ print params
        return params

