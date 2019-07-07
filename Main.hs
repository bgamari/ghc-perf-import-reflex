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
  => (Event t Text -> m (Event t [a]))
     -- ^ source of completions
  -> (Dynamic t a -> m (Event t b))
     -- ^ render completion
  -> (b -> Text)
     -- ^ render text of completion
  -> m (Dynamic t (Maybe b))
completer completions renderCompl complText = do
  rec
    input <- textInput $ def & textInputConfig_setValue .~ fmap complText setEvent
    let text = input ^. textInput_value
    compls <- completions $ ffilter (\t -> T.length t >= 2) $ updated text
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
  => Dynamic t (Maybe TestEnv)
  -> m (Dynamic t (Maybe CommitSha))
commitCompleter activeTestEnv = do
    completer completions renderCompl complText
  where
    completions :: Event t Text -> m (Event t [Commit])
    completions input =
      fmap (fmap $ fromMaybe $ error "error fetching completions")
      $ fetchCommitsWithPrefix
      $ attachWith ((,)) (current $ fmap (fromMaybe (TestEnv 0)) activeTestEnv) input

    renderCompl :: Dynamic t Commit -> m (Event t CommitSha)
    renderCompl commit = do
      (e, _) <- el' "li" $ do
        divClass "" $ dynText $ fmap (getCommitSha . commitSha) commit
        divClass "" $ dynText $ fmap (fromMaybe "the past" . commitDate) commit
        divClass "" $ dynText $ fmap (fromMaybe "commit title unavailable" . commitTitle) commit
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
  rec
    onLoadEvent <- button "hi"
    testEnvs <- do
      getTestEnvs onLoadEvent >>=
        holdDyn mempty . fmap (fromMaybe (error "error fetching test environments"))
      :: m (Dynamic t (M.Map TestEnv Text))
    activeTestEnv <- _dropdown_value <$> dropdown Nothing (fmap (M.mapKeys Just) testEnvs) def
    commit1 <- commitCompleter activeTestEnv
    commit2 <- commitCompleter activeTestEnv
    let dummy = fmap $ fromMaybe (CommitSha "")

    resultsTable (fromMaybe (TestEnv 0) <$> activeTestEnv) (dummy commit1) (dummy commit2)

  return ()

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

