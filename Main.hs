{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad.IO.Class
import Data.List (sortOn)
import qualified Data.Map          as M
import qualified Data.Text         as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Lens
import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Data.Functor.Misc
import Data.List (elem)
import Numeric
import Reflex.Dom

import QueryParams
import Db

completer
  :: forall t m a b.
     ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , MonadHold t m
     , PostBuild t m
     , TriggerEvent t m
     , HasJSContext (Performable m)
     , PerformEvent t m
     )
  => Event t Text
     -- ^ Event setting initial value
  -> (Dynamic t Text -> m (Event t [a]))
     -- ^ source of completions
  -> (Dynamic t a -> m (Event t b))
     -- ^ render completion
  -> (b -> Text)
     -- ^ render text of completion
  -> m (Dynamic t (Maybe b))
completer initial completions renderCompl complText = do
  rec
    input <- textInput $ def & textInputConfig_setValue .~ leftmost [fmap complText setEvent, initial]
    let text = input ^. textInput_value
    compls <- completions text
    compls' <- holdDyn [] $ compls
    setEvents <- el "ul" $ simpleList compls' renderCompl
    setEvent <- switchHold never $ updated $ fmap leftmost setEvents :: m (Event t b)

  holdDyn Nothing (fmap Just setEvent)

commitCompleter
  :: forall t m.
     ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , MonadHold t m
     , PostBuild t m
     , TriggerEvent t m
     , HasJSContext (Performable m)
     , PerformEvent t m
     )
  => Event t Text
  -> Dynamic t (Maybe TestEnv)
  -> m (Dynamic t (Maybe CommitSha))
commitCompleter initial activeTestEnv = do
  divClass "completer" $
    completer initial completions renderCompl complText
  where
    completions :: Dynamic t Text -> m (Event t [Commit])
    completions input =
      fmap (fmap $ fromMaybe $ error "error fetching completions")
      $ fetchCommitsWithPrefix
      $ ffilter (\(_, t) -> T.length t >= 1)
      $ updated
      $ ((,) <$> fmap (fromMaybe (TestEnv 0)) activeTestEnv <*> input)

    renderCompl :: Dynamic t Commit -> m (Event t CommitSha)
    renderCompl commit = do
      (e, _) <- el' "li" $ do
        elClass "span" "result-count" $ dynText
          $ fmap ((<> " results") . tshow . commitResultsCount) commit
        elClass "span" "commit-sha" $ dynText $ fmap (getCommitSha . commitSha) commit
        elClass "span" "commit-date" $ dynText $ fmap (fromMaybe "the past" . commitDate) commit
        divClass "commit-title" $ dynText $ fmap (fromMaybe "commit title unavailable" . commitTitle) commit
      return $ tag (commitSha <$> current commit) (select (_element_events e) (WrapArg Click))

    complText :: CommitSha -> Text
    complText = getCommitSha

resultsTable
  :: forall t m.
     ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , MonadHold t m
     , PostBuild t m
     , TriggerEvent t m
     , HasJSContext (Performable m)
     , PerformEvent t m
     )
  => Dynamic t TestEnv
  -> Dynamic t CommitSha
  -> Dynamic t CommitSha
  -> m ()
resultsTable testEnv commit1 commit2 = do
  let commitResults :: Dynamic t CommitSha -> m (Dynamic t (M.Map TestName Double))
      commitResults c = do
          rs <- getCommitResults
                $ updated
                $ (,) <$> c <*> testEnv
          holdDyn mempty $ fmap f rs
        where f :: Maybe [Result] -> M.Map TestName Double
              f Nothing = error "error fetching results"
              f (Just xs) = M.fromList [ (testName x, resultValue x) | x <- xs ]

  commit1Res <- commitResults commit1
  commit2Res <- commitResults commit2
  let results :: Dynamic t [(TestName, Double, (Double, Double))]
      results = sortedDeltas <$> commit1Res <*> commit2Res
  elClass "table" "table is-striped" $ do
    tr $ mapM_ (el "th")
      [ text "test name"
      , commitShaT commit1
      , commitShaT commit2
      , text "relative change"
      ]
    let toRow :: Dynamic t (TestName, Double, (Double, Double)) -> m ()
        toRow r = tr $ do
          td $ dynText $ fmap (view $ _1 . to getTestName) r
          td $ dynText $ fmap (view $ _3 . _1 . to showValue) r
          td $ dynText $ fmap (view $ _3 . _2 . to showValue) r
          pctChangeCell $ fmap (view $ _2 . to (*100)) r

        pctChangeCell :: Dynamic t Double -> m ()
        pctChangeCell x =
            elDynClass "td" (fmap cls x) $ dynText $ fmap ((<> "%") . tshowReal 1) x
          where
            cls pctChange
              | pctChange >  0.1 = "increase"
              | pctChange < -0.1 = "decrease"
              | otherwise     = ""
        showValue = tshowReal 3
    el "tbody" $ simpleList results toRow
  return ()

  where
    tr = el "tr"
    td = el "td"
    commitShaT :: Dynamic t CommitSha -> m ()
    commitShaT = dynText . fmap (getCommitSha)

app
  :: forall t m.
     ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadIO m
     , MonadIO (Performable m)
     , MonadHold t m
     , PostBuild t m
     , TriggerEvent t m
     , HasJSContext (Performable m)
     , PerformEvent t m
     )
  => m ()
app = divClass "container" $ do
  elClass "h1" "title" $ text "GHC Performance Statistics Browser"
  queryParams <- queryParamsEvent
  let getInitialField :: Text -> Text -> Event t Text
      getInitialField def k = fmap (fromMaybe def . lookup k) queryParams

  rec
    onLoadEvent <- getPostBuild
    (activeTestEnv, commit1, commit2) <- elClass "section" "section" $ do
      testEnvs <- do
        getTestEnvs onLoadEvent >>=
          holdDyn mempty . fmap (fromMaybe (error "error fetching test environments"))
        :: m (Dynamic t (M.Map TestEnv Text))
      let initialTestEnv = fmap (fmap (TestEnv . read . T.unpack) . lookup "test_env") queryParams
      activeTestEnv <- labelledField "Test environment" $ do
        _dropdown_value <$> dropdown Nothing (fmap (M.mapKeys Just) testEnvs) def{ _dropdownConfig_setValue = initialTestEnv }

      commit1 <- labelledField "Commit 1" $ commitCompleter (getInitialField "" "commit1") activeTestEnv
      commit2 <- labelledField "Commit 2" $ commitCompleter (getInitialField "" "commit2") activeTestEnv
      return (activeTestEnv, commit1, commit2)

    let dummy = fmap $ fromMaybe (CommitSha "")
    resultsTable (fromMaybe (TestEnv 0) <$> activeTestEnv) (dummy commit1) (dummy commit2)

  return ()

labelledField :: (DomBuilder t m) => Text -> m a -> m a
labelledField label field =
  elClass "field" "field" $ do
    elClass "label" "label" $ text label
    divClass "control" field

tshow :: Show a => a -> Text
tshow = T.pack . show

tshowReal :: RealFloat a => Int -> a -> Text
tshowReal n x = T.pack $ showFFloat (Just n) x ""

htmlHead :: DomBuilder t m => m ()
htmlHead = do
    elAttr "link" (mconcat
        [ "href" =: "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"
        , "rel" =: "stylesheet"
        , "type" =: "text/css"
        , "crossorigin" =: "anonymous"
        ]) (return ())
    elAttr "link" (mconcat
        [ "href" =: "style.css"
        , "rel" =: "stylesheet"
        , "type" =: "text/css"
        ]) (return ())
    elAttr "meta" (mconcat
        [ "name" =: "viewport"
        , "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no"
        ]) (return ())
    elAttr "meta" (mconcat
        [ "charset" =: "utf-8"
        ]) (return ())

    el "title" (text "GHC Performance")

main :: IO ()
main = mainWidgetWithHead htmlHead app

sortedDeltas :: Ord a => M.Map a Double -> M.Map a Double -> [(a, Double, (Double, Double))]
sortedDeltas xs ys =
    sortOn (\(_,rel,_) -> rel)
    [ (k, relChange v1 v2, (v1, v2))
    | (k, (v1, v2)) <- M.toList $ M.intersectionWith (,) xs ys
    ]

relChange :: Double -> Double -> Double
relChange x y = (y - x) / x

