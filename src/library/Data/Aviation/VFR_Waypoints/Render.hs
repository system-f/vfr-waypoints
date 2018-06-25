{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.VFR_Waypoints.Render {- (

) -} where

import Control.Lens
import Data.Aviation.VFR_Waypoints
import Data.List
import Prelude
import qualified Text.Fuzzy as Fuzzy
import Text.Fuzzy(Fuzzy)
import Text.Printf

toDecimal ::
  (Integral a, Integral b, Ord c, Fractional c) =>
  (a, b, c) ->
  c
toDecimal (x, y, z) =
  let x' = fromIntegral x
      (.?.) = if x' < 0 then (-) else (+)
  in  x' .?. (fromIntegral y/60 + z/60)

mkN ::
  Int
  -> String
  -> String
mkN n x =
  let n' = n - length (take n x)
  in  x ++ replicate n' ' '

renderVFR_WaypointSeparator ::
  String
renderVFR_WaypointSeparator =
  "\ESC[34m\ESC[44m \ESC[m"

renderVFR_WaypointHeader ::
  String
renderVFR_WaypointHeader =
  let colour x =
        concat
          [
            "\ESC[31m\ESC[47m"
          , x
          , "\ESC[m"
          ]
  in  intercalate renderVFR_WaypointSeparator . fmap colour $
        [
          mkN 32 "WAYPOINT"
        , "STATE"
        , mkN 5 "CODE"
        , mkN 22 "LAT"
        , mkN 22 "LON"
        , "SCORE"
        ]

renderVFR_Waypoint ::
  HasVFR_Waypoint w =>
  Fuzzy w String
  -> String
renderVFR_Waypoint w' =
  let w =
        Fuzzy.original w'
      white_black x =
        concat
          [
            "\ESC[40m\ESC[37m"
          , x
          , "\ESC[m"
          ]
      name' =
        w ^. name
      state' =
        w ^. state
      code' =
        w ^. code
      lat' =
        w ^. lat
      lon' =
        w ^. lon
  in  intercalate renderVFR_WaypointSeparator . fmap (white_black =<<) $
        [
          [
            mkN 32 name'
          ]
        , [
            case state' of
              Nothing ->
                ""
              Just s ->
                mkN 5 s
          ]
        , [
            mkN 5 code'
          ]
        , [
            printf "%03d" (lat' ^. latitudeDegrees)
          , " "
          , printf "%02d" (lat' ^. latitudeMinutes)
          , drop 1 . printf "%.1f" $ (lat' ^. latitudeMantissa)
          , "    "
          , printf "%.6f" (toDecimal (lat' ^. latitudeDegrees, lat' ^. latitudeMinutes, lat' ^. latitudeMantissa))
          ]
        , [
            show (lon' ^. longitudeDegrees)
          , " "
          , printf "%02d" (lon' ^. longitudeMinutes)
          , drop 1 . printf "%.1f" $ (lon' ^. longitudeMantissa)
          , "    "
          , printf "%.6f" (toDecimal (lon' ^. longitudeDegrees, lon' ^. longitudeMinutes, lon' ^. longitudeMantissa))
          ]
        , [
             mkN 5 (show (Fuzzy.score w'))
          ]
        ]