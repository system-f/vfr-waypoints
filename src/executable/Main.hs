module Main(
  main
) where

import Control.Applicative
import Data.Aviation.VFR_Waypoints.Render
import Data.Aviation.VFR_Waypoints.Search

import System.Environment
import Text.Fuzzy(Fuzzy(Fuzzy))

main ::
  IO ()
main =
  do  a <- getArgs
      case a of
        [] ->
          putStrLn "args"
        h:_ ->
          let rs = (\(Fuzzy o _ s) -> (o, show s)) <$> searchFuzzyCodeName h "" "" False
              ps = (>>= (++ "\n")) <$> traverse (\x -> renderVFR_Waypoint x) rs
              z = liftA2 (\hd w -> hd ++ "\n" ++ w) renderVFR_WaypointHeader ps
          in  putStrLn (runColour z True)

-- code, name or both
-- colours on/off
-- exact match
-- copy URL to clipboard