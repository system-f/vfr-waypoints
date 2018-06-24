{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.VFR_Waypoints.Render {- (

) -} where

import Control.Lens
import Data.Aviation.VFR_Waypoints
import Prelude
import Text.Printf

renderVFR_Waypoint ::
  HasVFR_Waypoint w =>
  w
  -> String
renderVFR_Waypoint w =
  let yellow_green x =
        concat
          [
            "\ESC[92m\ESC[42m"
          , x
          , "\ESC[m"
          ]
      white_red x =
        concat
          [
            "\ESC[38m\ESC[41m"
          , x
          , "\ESC[m"
          ]
      light_pink x =
        concat
          [
            "\ESC[95m\ESC[45m"
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
          white_red "WAYPOINT"
        , white_black (mkN 32 name')
        , yellow_green " "
        , case state' of
            Nothing ->
              ""
            Just s ->
              concat
                [
                  white_red "STATE"
                , white_black (mkN 3 s)
                , yellow_green " "
                ]
        , white_red "CODE"
        , white_black (mkN 5 code')
        , yellow_green " "
        , white_red "LAT"
        , white_black (show (lat' ^. latitudeExponent))
        , white_black " "
        , white_black (printf "%04.1f" (lat' ^. latitudeMantissa))
        , yellow_green " "
        , white_red "LON"
        , white_black (show (lon' ^. longitudeExponent))
        , white_black " "
        , white_black (printf "%04.1f" (lon' ^. longitudeMantissa))
        ]
