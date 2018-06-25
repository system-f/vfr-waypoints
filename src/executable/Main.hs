module Main(
  main
) where

import Data.Aviation.VFR_Waypoints
import Data.Aviation.VFR_Waypoints.Render
import Data.Aviation.VFR_Waypoints.Search

import System.Environment
import qualified Text.Fuzzy as Fuzzy

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        [] ->
          putStrLn "args"
        h:_ ->
          let rs = searchFuzzyCodeName h "" "" False
              s = rs >>= \r -> renderVFR_Waypoint r ++ "\n"
          in  do  putStrLn renderVFR_WaypointHeader
                  putStrLn s
