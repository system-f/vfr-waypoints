{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.VFR_Waypoints.Search where

import Control.Lens
import Data.Aviation.VFR_Waypoints
import Data.Char
import Data.Map(Map)
import qualified Data.Map as Map
import Prelude
import Text.Regex

all_VFR_Waypoint_codes_index ::
  Map String (String, Maybe String, Double, Double)
all_VFR_Waypoint_codes_index =
  Map.fromList ((\(VFR_Waypoint _name _state _code _lat _lon) -> (_code,  (_name, _state, _lat, _lon))) <$> all_VFR_Waypoint ^. _Wrapped)

all_VFR_Waypoint_names_index ::
  Map String (Maybe String, String, Double, Double)
all_VFR_Waypoint_names_index =
  Map.fromList ((\(VFR_Waypoint _name _state _code _lat _lon) -> (_name,  (_state, _code, _lat, _lon))) <$> all_VFR_Waypoint ^. _Wrapped)

searchIndexCode ::
  String
  -> Maybe VFR_Waypoint
searchIndexCode s =
  let s' = filter isAlpha . fmap toUpper $ s
  in  (\(_name, _state, _lat, _lon) -> VFR_Waypoint _name _state s' _lat _lon) <$> Map.lookup s' all_VFR_Waypoint_codes_index

searchRegex ::
  String
  -> [VFR_Waypoint]
searchRegex =
  let r = (\(VFR_Waypoint _name _state _code _lat _lon) -> undefined) <$> all_VFR_Waypoint ^. _Wrapped
  in  undefined

-- https://hackage.haskell.org/package/fuzzy

{-

all_VFR_Waypoint ::
  VFR_Waypoints
all_VFR_Waypoint =

-}

