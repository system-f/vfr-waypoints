{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.VFR_Waypoints.Render {- (

) -} where

import Control.Lens
import Data.Aviation.VFR_Waypoints
import Prelude
import Text.Printf

toDecimal ::
  (Integral a, Integral b, Ord c, Fractional c) =>
  (a, b, c) ->
  c
toDecimal (x, y, z) =
  let x' = fromIntegral x
      (.?.) = if x' < 0 then (-) else (+)
  in  x' .?. (fromIntegral y/60 + z/60)

renderVFR_Waypoint ::
  HasVFR_Waypoint w =>
  w
  -> String
renderVFR_Waypoint w =
  let red_white x =
        concat
          [
            "\ESC[31m\ESC[47m"
          , x
          , "\ESC[m"
          ]
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
      mkN n x =
        let n' = n - length (take n x)
        in  x ++ replicate n' ' '
  in  concat
        [
          red_white "WAYPOINT"
        , white_black (mkN 32 name')
        , case state' of
            Nothing ->
              ""
            Just s ->
              concat
                [
                  red_white "STATE"
                , white_black (mkN 3 s)
                ]
        , red_white "CODE"
        , white_black (mkN 5 code')
        , red_white "LAT"
        , white_black (printf "%03d" (lat' ^. latitudeDegrees))
        , white_black " "
        , white_black (printf "%02d" (lat' ^. latitudeMinutes))
        , white_black (drop 1 . printf "%.1f" $ (lat' ^. latitudeMantissa))
        , white_black "    "
        , white_black (printf "%.6f" (toDecimal (lat' ^. latitudeDegrees, lat' ^. latitudeMinutes, lat' ^. latitudeMantissa)))
        , red_white "LON"
        , white_black (show (lon' ^. longitudeDegrees))
        , white_black " "
        , white_black (printf "%02d" (lon' ^. longitudeMinutes))
        , white_black (drop 1 . printf "%.1f" $ (lon' ^. longitudeMantissa))
        , white_black "    "
        , white_black (printf "%.6f" (toDecimal (lon' ^. longitudeDegrees, lon' ^. longitudeMinutes, lon' ^. longitudeMantissa)))
        ]
