{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Data.List (nub)
import Data.Text hiding (length)
import qualified Data.Text.Builder.Linear as TBL
import Data.Text.IO
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Template.Builder as Builder
import qualified Data.Text.Template.Closured as Closured
import qualified Data.Text.Template.LinearBuilder as LinearBuilder
import qualified Data.Text.Template.Parse as Parse
import qualified Data.Text.Template.StrictText as StrictText
import Example
import Language.Haskell.TH.Lift
import Test.Tasty.Bench
import Text.RawString.QQ
import Prelude hiding (readFile)

main :: IO ()
main = do
    fileContent <- readFile "./bench/template.xml"
    let staticExample1 = $(lift (Parse.cacheLocalityOptimise $ Parse.template example1 :: Parse.Template))
    let results =
            nub
                [ (`StrictText.render` id) staticExample1
                , TBL.runBuilder . (`Closured.render` TBL.fromText) $ Closured.template fileContent
                , TBL.runBuilder . (`Closured.render` TBL.fromText) $ Closured.templateConvert staticExample1
                , TBL.runBuilder . (`Closured.render` TBL.fromText) $ Closured.templateConvertOptimized staticExample1
                , TBL.runBuilder . (`Closured.renderLazy` TBL.fromText) $ Closured.template fileContent
                , TBL.runBuilder . (`Closured.render` TBL.fromText) $ Closured.template fileContent
                , TBL.runBuilder . (`LinearBuilder.render` TBL.fromText) $ LinearBuilder.template fileContent
                , TBL.runBuilder . (`LinearBuilder.render` TBL.fromText) $ LinearBuilder.templateConvertOptimized . Parse.template $ fileContent
                , TBL.runBuilder . (`LinearBuilder.render` TBL.fromText) $ LinearBuilder.templateConvert staticExample1
                , TBL.runBuilder . (`LinearBuilder.render` TBL.fromText) $ LinearBuilder.templateConvertOptimized staticExample1
                ]
    print (length results)
    -- print results
    defaultMain
        [ bench "StrictText.renderA"
            $ nfAppIO (`StrictText.renderA` (const $ pure "qqqqqqqqqqqqqqq")) staticExample1
        , bench "Closured.renderA"
            $ nfAppIO (\template' -> TBL.runBuilder <$> Closured.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ Closured.template fileContent
        , bench "Closured.renderA static"
            $ nfAppIO (\template' -> TBL.runBuilder <$> Closured.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ Closured.templateConvert staticExample1
        , bench "Closured.renderA static optimized"
            $ nfAppIO (\template' -> TBL.runBuilder <$> Closured.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ Closured.templateConvertOptimized staticExample1
        , bench "Closured.render lazy"
            $ nf (\template' -> TBL.runBuilder $ Closured.renderLazy template' (const $ "qqqqqqqqqqqqqqq"))
            $ Closured.template fileContent
        , bench "Closured.render"
            $ nf (\template' -> TBL.runBuilder $ Closured.render template' (const $ "qqqqqqqqqqqqqqq"))
            $ Closured.template fileContent
        , bench "LinearBuilder.renderA"
            $ nfAppIO (\template' -> TBL.runBuilder <$> LinearBuilder.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ LinearBuilder.template fileContent
        , bench "LinearBuilder.renderA optimized"
            $ nfAppIO (\template' -> TBL.runBuilder <$> LinearBuilder.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ LinearBuilder.templateConvertOptimized
            . Parse.template
            $ fileContent
        , bench "LinearBuilder.renderA static"
            $ nfAppIO (\template' -> TBL.runBuilder <$> LinearBuilder.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ LinearBuilder.templateConvert staticExample1
        , bench "LinearBuilder.renderA BS"
            $ nfAppIO (\template' -> TBL.runBuilderBS <$> LinearBuilder.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ LinearBuilder.templateConvert staticExample1
        , bench "LinearBuilder.renderA static optimized"
            $ nfAppIO (\template' -> TBL.runBuilder <$> LinearBuilder.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ LinearBuilder.templateConvertOptimized staticExample1
        , bench "Builder.renderA"
            $ nfAppIO (\template' -> TB.toLazyText <$> Builder.renderA template' (const $ pure "qqqqqqqqqqqqqqq"))
            $ Builder.template example1
        ]
