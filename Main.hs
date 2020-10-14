{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

import Reflex.Dom

import qualified Compare

main :: IO ()
main = mainWidgetWithHead htmlHead Compare.app

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

