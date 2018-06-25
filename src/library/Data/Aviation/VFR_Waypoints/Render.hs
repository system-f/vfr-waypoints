{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.VFR_Waypoints.Render (
  Colour(..)
, runColour
, colour
, renderVFR_WaypointSeparator
, renderVFR_WaypointHeader
, renderVFR_Waypoint
) where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category((.))
import Control.Lens((^.))
import Control.Monad(Monad(return, (>>=)))
import Data.Aviation.VFR_Waypoints(HasVFR_Waypoint(name, state, code, lat, lon), latitudeDegrees, latitudeMinutes, latitudeMantissa, longitudeDegrees, longitudeMinutes, longitudeMantissa)
import Data.Bool(Bool)
import Data.Foldable(length)
import Data.Function(($))
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.List(intercalate, take, drop, (++), replicate, concat)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord((<)))
import Data.String(String)
import Data.Traversable(traverse)
import Prelude(Integral, Fractional, (-), (+), (/), fromIntegral, show)
import Text.Printf(printf)

mkN ::
  Int
  -> String
  -> String
mkN n x =
  let n' = n - length (take n x)
  in  x ++ replicate n' ' '

data Colour a =  
  Colour (Bool -> a)

runColour ::
  Colour a
  -> Bool
  -> a
runColour (Colour x) =
  x

instance Functor Colour where
  fmap f (Colour g) =
    Colour (f . g)

instance Applicative Colour where
  pure =
    Colour . pure
  Colour f <*> Colour a =
    Colour (f <*> a)

instance Monad Colour where
  return =
    pure
  Colour x >>= f =
    Colour (\p -> runColour (f (x p)) p)

colour ::
  String
  -> String
  -> Colour String
colour s c =
  Colour (\p ->
    if p
      then
        concat
          [
            c
          , s
          , "\ESC[m"
          ]
      else
        s)

renderVFR_WaypointSeparator ::
  Colour String
renderVFR_WaypointSeparator =
  colour " " "\ESC[34m\ESC[44m"
  
renderVFR_WaypointHeader ::
  Colour String
renderVFR_WaypointHeader =
  do  s <- renderVFR_WaypointSeparator
      y <- traverse (`colour` "\ESC[31m\ESC[47m")
            [
              mkN 32 "WAYPOINT"
            , "STATE"
            , mkN 5 "CODE"
            , mkN 8 "LAT"
            , mkN 8 "LON"
            , mkN 83 "openstreetmap.org"
            , "SCORE"
            ]
      pure (intercalate s y)

renderVFR_Waypoint ::
  HasVFR_Waypoint w =>
  (w, String)
  -> Colour String
renderVFR_Waypoint (w, sc) =
  let name' =
        w ^. name
      state' =
        w ^. state
      code' =
        w ^. code
      lat' =
        w ^. lat
      lon' =
        w ^. lon
      toDecimal ::
        (Integral a, Integral b, Ord c, Fractional c) =>
        (a, b, c) ->
        c
      toDecimal (x, y, z) =
        let x' = fromIntegral x
            (.?.) = if x' < 0 then (-) else (+)
        in  x' .?. (fromIntegral y/60 + z/60)

  in  do  s <- renderVFR_WaypointSeparator
          y <- traverse ((`colour` "\ESC[40m\ESC[37m") . concat) $
                  [
                    [
                      mkN 32 name'
                    ]
                  , [
                      mkN 5 $
                        case state' of
                          Nothing ->
                            ""
                          Just st ->
                            mkN 5 st
                    ]
                  , [
                      mkN 5 code'
                    ]
                  , [
                      printf "%03d" (lat' ^. latitudeDegrees)
                    , " "
                    , printf "%02d" (lat' ^. latitudeMinutes)
                    , drop 1 . printf "%.1f" $ (lat' ^. latitudeMantissa)
                    ]
                  , [
                      show (lon' ^. longitudeDegrees)
                    , " "
                    , printf "%02d" (lon' ^. longitudeMinutes)
                    , drop 1 . printf "%.1f" $ (lon' ^. longitudeMantissa)
                    ]
                  , let lat'' =
                          printf "%08.4f" (toDecimal (lat' ^. latitudeDegrees, lat' ^. latitudeMinutes, lat' ^. latitudeMantissa))
                        lon'' =
                          printf "%08.4f" (toDecimal (lon' ^. longitudeDegrees, lon' ^. longitudeMinutes, lon' ^. longitudeMantissa))
                    in  [
                          "https://www.openstreetmap.org/?mlat="
                        , lat''
                        , "&mlon="
                        , lon''
                        , "#map=14/"
                        , lat''
                        , "/"
                        , lon''
                        ]
                  , [
                       mkN 5 sc
                    ]
                  ]
          pure (intercalate s y)
