{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module Main(
  main
) where

import Control.Applicative(pure, (<*>), (<**>))
import Control.Category((.))
import Data.Aviation.VFR_Waypoints.Render(renderVFR_Waypoint, render0ResultsOr, render0ResultsList, renderVFR_WaypointHeader, runColour)
import Data.Aviation.VFR_Waypoints.Search(searchIndexCode, searchIndexName, searchIndexCodeName, searchFuzzyCode, searchFuzzyName, searchFuzzyCodeName)
import Data.Bool(Bool(True, False), not)
import Data.Eq(Eq)
import Data.Function(($))
import Data.Functor((<$>), fmap)
import Data.Int(Int)
import Data.List(filter, (++))
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord((>=)))
import Data.Semigroup((<>))
import Data.String(String)
import Options.Applicative(Parser, execParser, info, helper, fullDesc, header, option, maybeReader, short, long, value, metavar, help, switch, strOption)
import Prelude(Show(show))
import System.IO(IO, putStrLn)
import Text.Fuzzy(Fuzzy(Fuzzy))
import Text.Read(reads)

main ::
  IO ()
main =
  let opts =
        execParser
          (info (parserOptions <**> helper) (
            fullDesc <>
            header ("vfr-waypoints " <> VERSION_vfr_waypoints <> " for searching VFR waypoints")
          )
        )
  in  do  Options c h m t x <- opts
          let output h' =
                case t of
                  Exact ->
                    let k =
                          case m of
                            Code ->
                              searchIndexCode
                            Name ->
                              searchIndexName
                            Both ->
                              searchIndexCodeName
                        render w =
                          renderVFR_Waypoint (w, "-")
                    in  render0ResultsOr h'
                          (render <$> k x)
                  Inexact sc ->
                    let k =
                          case m of
                            Code ->
                              searchFuzzyCode
                            Name ->
                              searchFuzzyName
                            Both ->
                              searchFuzzyCodeName
                        render (Fuzzy o _ s) =
                          renderVFR_Waypoint (o, show s)
                        scores (Fuzzy _ _ s) =
                          case sc of
                            Nothing ->
                              True
                            Just sc' ->
                              s >= sc'
                    in  render0ResultsList h'
                          (
                            fmap render . 
                            filter scores $
                            (k x "" "" False)
                          )
              decideheader2 =
                if h
                  then
                    pure ""
                  else
                    (++ "\n") <$> renderVFR_WaypointHeader
                  
              alloutput =
                output decideheader2
          putStrLn (runColour alloutput (not c))
          
data MatchOptions =
  Code
  | Name
  | Both
  deriving (Eq, Ord, Show)

parserMatchOptions ::
  Parser MatchOptions
parserMatchOptions =
  option
    (maybeReader (\s -> case s of
                          "code" ->
                            Just Code
                          "name" ->
                            Just Name
                          "both" ->
                            Just Both
                          _      ->
                            Nothing))
    (
      short 'm' <>
      long "match" <>
      value Both <>
      metavar "(code|name|both)" <>
      help "Search on VFR waypoint code, name or both code and name"
    )

data MatchType =
  Exact
  | Inexact (Maybe Int)
  deriving (Eq, Ord, Show)

parserMatchType ::
  Parser MatchType
parserMatchType =
  let opts exact minscore =
        if exact
          then
            Exact
          else
            Inexact minscore
  in  opts <$>
      switch
        (
          short 'e' <>
          long "exact" <>
          help "match the search term exactly"
        ) <*>
      option
        (
          maybeReader
            (\s -> case reads s of
                      (n, _):_ ->
                        Just (Just n)
                      [] ->
                        Nothing)
        )
        (
          short 's' <>
          long "min-score" <>
          value Nothing <>
          help "minimum fuzzy match score"
        )

data Options =
  Options
    Bool -- colours
    Bool -- header
    MatchOptions
    MatchType
    String
  deriving (Eq, Ord, Show)

parserOptions ::
  Parser Options
parserOptions =
  Options <$>
  switch
    (
      short 'r' <>
      long "colour" <>
      help "turn off colouring with ANSI escape code"
    ) <*>
  switch
    (
      short 'h' <>
      long "header" <>
      help "turn off header in search results"
    ) <*>
  parserMatchOptions <*>
  parserMatchType <*>
  strOption
    (
      short 't' <>
      long "term" <>
      help "the search term"
    )
