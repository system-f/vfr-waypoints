{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.VFR_Waypoints.Search(
  all_VFR_Waypoint_codes_index
, all_VFR_Waypoint_names_index
, searchIndexCode
, searchIndexName
, searchIndexCodeName
, searchFuzzyCode
, searchFuzzyName
, searchFuzzyCodeName
) where

import Control.Applicative((<|>))
import Control.Category((.))
import Control.Lens(_Wrapped, (^.))
import Data.Aviation.VFR_Waypoints(VFR_Waypoint(VFR_Waypoint), Latitude, Longitude, all_VFR_Waypoint, code, name)
import Data.Bool(Bool)
import Data.Char(isAlpha, toUpper)
import Data.Foldable(foldl')
import Data.Functor((<$>), fmap)
import Data.Function(($))
import Data.List(sortBy, filter)
import Data.Map(Map)
import qualified Data.Map as Map(fromList, lookup, insertWith, toList)
import Data.Maybe(Maybe)
import Data.Monoid.Textual(TextualMonoid)
import Data.Ord(Ord((>), compare))
import Data.String(String)
import qualified Text.Fuzzy as Fuzzy(filter, score)
import Text.Fuzzy(Fuzzy(Fuzzy))

all_VFR_Waypoint_codes_index ::
  Map String (String, Maybe String, Latitude, Longitude)
all_VFR_Waypoint_codes_index =
  Map.fromList ((\(VFR_Waypoint _name _state _code _lat _lon) -> (_code,  (_name, _state, _lat, _lon))) <$> all_VFR_Waypoint ^. _Wrapped)

all_VFR_Waypoint_names_index ::
  Map String (Maybe String, String, Latitude, Longitude)
all_VFR_Waypoint_names_index =
  Map.fromList ((\(VFR_Waypoint _name _state _code _lat _lon) -> (_name,  (_state, _code, _lat, _lon))) <$> all_VFR_Waypoint ^. _Wrapped)

searchIndexCode ::
  String
  -> Maybe VFR_Waypoint
searchIndexCode s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_name, _state, _lat, _lon) -> VFR_Waypoint _name _state s' _lat _lon) <$> Map.lookup s' all_VFR_Waypoint_codes_index

searchIndexName ::
  String
  -> Maybe VFR_Waypoint
searchIndexName s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_state, _code, _lat, _lon) -> VFR_Waypoint s' _state _code _lat _lon) <$> Map.lookup s' all_VFR_Waypoint_names_index

searchIndexCodeName ::
  String
  -> Maybe VFR_Waypoint
searchIndexCodeName s =
  searchIndexCode s <|> searchIndexName s

searchFuzzyCode ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy VFR_Waypoint String]
searchFuzzyCode s before after cas =
  Fuzzy.filter s (all_VFR_Waypoint ^. _Wrapped) before after (^. code) cas

searchFuzzyName ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy VFR_Waypoint String]
searchFuzzyName s before after cas =
  Fuzzy.filter s (all_VFR_Waypoint ^. _Wrapped) before after (^. name) cas

searchFuzzyCodeName ::
  String
  -> String
  -> String
  -> Bool
  -> [Fuzzy VFR_Waypoint String]
searchFuzzyCodeName s before after cas =
  filter2 s (all_VFR_Waypoint ^. _Wrapped) before after ((^. code), (^. name)) cas

-- https://hackage.haskell.org/package/fuzzy-0.1.0.0/docs/Text-Fuzzy.html#v:filter
filter2 ::
  (Ord t, TextualMonoid s) =>
  s
  -> [t]
  -> s
  -> s
  -> (t -> s, t -> s)
  -> Bool
  -> [Fuzzy t s]
filter2 pattern values before after (extract1, extract2) cas =
  let x1 = Fuzzy.filter pattern values before after extract1 cas
      x2 = Fuzzy.filter pattern values before after extract2 cas
      x1' = Map.fromList ((\(Fuzzy o r s) -> (o, (r, s))) <$> x1)
      x2' = foldl' (\m (Fuzzy o r s) -> Map.insertWith (\(s1, i1) (s2, i2) -> if i2 > i1 then (s2, i2) else (s1, i1)) o (r, s) m) x1' x2
  in  sortBy (\f1 f2 -> Fuzzy.score f2 `compare` Fuzzy.score f1) ((\(o, (r, s)) -> Fuzzy o r s) <$> Map.toList x2')
