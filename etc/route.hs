-- 1. open VFR pdf from AIP with libreoffice
-- 2. export as HTML

{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Data.Char
import Data.List
import Data.List.NonEmpty(NonEmpty)
import Data.Maybe
import System.Environment
import Text.Printf

data Coordinate =
  Coordinate
    Double
    Double

data Waypoint =
  Waypoint
    String -- name
    String -- code
    (Maybe Coordinate)

newtype Runway =
  Runway
    String

data WaypointArrivalType =
  FullStop (Maybe Runway)
  | TouchAndGo (Maybe Runway)
  | StopAndGo (Maybe Runway)
  | GoAround (Maybe Runway)
  | Overfly

data WaypointArrival =
  WaypointArrival
    Waypoint
    WaypointArrivalType
    (NonEmpty WaypointArrivalType)

newtype Route =
  Route
    [WaypointArrival]

breaks ::
  (a -> Bool)
  -> [a]
  -> [[a]]
breaks p x =
  let (a, b) = break p x
  in  a :
        (
          case b of
            [] ->
              []
            (_':bs) -> 
              breaks p bs
        )

removeCarriageReturn ::
  [String]
  -> [String]
removeCarriageReturn =
  let r [] =
        []
      r ('\r':t) =
        t
      r s =
        s
  in  fmap (reverse . r . reverse)

lines' ::
  String
  -> [String]
lines' =
  removeCarriageReturn . lines

parse ::
  String
  -> [[String]]
parse =
  let unp s =
        case s of
          '<':'p':'>':t ->
            case reverse t of
              '>':'p':'/':'<':u ->
                reverse u
              _ ->
                s
          _ ->
            s
      remove x =
        or [
          x `elem` [
            "<p><b>VFR WAYPOINTS - ENCODED</b></p>"
          , "<p><i><b>WAYPOINT</b></i></p>"
          , "<p><i><b>STATE</b></i></p>"
          , "<p><i><b>CODE</b></i></p>"
          , "<p><i><b>LAT</b></i></p>"
          , "<p><i><b>LONG</b></i></p>"
          , "<h1 style=\"page-break-before:always; \"></h1>"
          , "<p>This page is intentionally blank.</p>"
          , "</body>"
          , "</html>"
          ]
        , "<p>VFR - GEN" `isPrefixOf` x
        , "<p><font color=\"#FFFFFF\"" `isPrefixOf` x
        , case x of
            '<':'p':'>':_:_:' ':_:_:_:' ':'2':'0':_:_:'<':'/':'p':'>':[] -> True
            _ -> False
        ]
  in  map (map unp . filter (not . remove)) .
      filter (not . isSuffixOf ["<h1></h1>"]) .
      breaks (== "<p>VFR WAYPOINTS</p>") .
      lines'

parse' ::
  String
  -> [String]
parse' x =
  concat .
  fst .
  fmap (\b' ->  case b' of
                  [] ->
                    []
                  h:t ->
                    drop 1 h:t) . 
  break (\x -> take 1 x == ["<b>VFR WAYPOINTS - DECODED</b>"]) .
              parse $ x

data WaypointEncoded =
  WaypointEncoded
    String -- WAYPOINT
    (Maybe String) -- STATE
    String -- CODE
    (Int, Int, Double) -- LAT
    (Int, Int, Double) -- LON
  deriving (Eq, Ord, Show)

waypointCode (WaypointEncoded _ _ c _ _) =
  c

parselatlon ::
  String
  -> Maybe (Int, Int, Double)
parselatlon (c:' ':r) =
  let w = if c `elem` "SW"
            then Just (-1)
            else if c `elem` "NE"
              then Just 1
              else Nothing
      j =
        reads r >>= \(i', s) -> 
        let (v, w) = break (== '.') s
        in pure (i', read v, read ('0':w))
  in  do  w'           <- w
          (i', j', k') <- listToMaybe j
          pure (w' * i', j', k')
parselatlon _ =
  Nothing

encode ::
  [String]
  -> Maybe [WaypointEncoded]
encode (wpt:x1:x2:x3:rest) =
  let t1 =
        do  lat' <- parselatlon x2
            lon' <- parselatlon x3
            r'   <- encode rest
            pure (WaypointEncoded wpt Nothing x1 lat' lon' : r')
      t2 =
        case rest of
          r:rs ->
            do  lat' <- parselatlon x3
                lon' <- parselatlon r
                r'   <- encode rs
                pure (WaypointEncoded wpt (Just x1) x2 lat' lon' : r')
          [] ->
            Nothing
  in t1 <|> t2
encode x =
  Just []

render ::
  [WaypointEncoded]
  -> String
render ws =
  let name_wpt w =
        concat ["_", w, "_"]
      paren p s =
        if p then concat ["(", s, ")"] else s
      render1 (WaypointEncoded wpt x1 x2 (x3, x3', x3'') (x4, x4', x4'')) =
        concat
          [

            name_wpt x2
          , " ::\n"
          , "  VFR_Waypoint\n"
          , name_wpt x2
          , " =\n"
          , "  VFR_Waypoint\n"
          , "    "
          , show wpt
          , "\n    "
          , paren (isJust x1) (show x1)
          , "\n    "
          , show x2
          , "\n    "
          , "(Lat "
          , paren (x3<0) (show x3)
          , " "
          , show x3'
          , " "
          , printf "%.1f" x3''
          , ")"
          , "\n    "
          , "(Lon "
          , paren (x4<0) (show x4)
          , " "
          , show x4'
          , " "
          , printf "%.1f" x4''
          , ")\n"
          ]
      all_wpts w =
        let str s =
             concat
                [
                  "all_VFR_Waypoint ::\n"
                , "  VFR_Waypoints\n"
                , "all_VFR_Waypoint =\n"
                , "  VFR_Waypoints\n"
                , "    [\n"
                , s
                , "    ]\n"
                ]
            wpts [] =
              str ""
            wpts (x:xs) =
              str $
                concat
                  [
                    "      "
                  , name_wpt (waypointCode x)
                  , "\n"
                  , xs >>= \g ->
                      concat
                        [
                          "    , "
                        , name_wpt (waypointCode g)
                        , "\n"
                        ]
                  ]
        in  wpts w
  in  concat
        [
          intercalate "\n"
            (render1 <$> ws)
        , "\n"
        , all_wpts ws
        ]

main ::
  IO ()
main =
  do
      a <- getArgs
      case a of
        [] ->
          putStrLn "arguments: <VFR waypoints.html> <output-file>"
        inp:out:_ ->
          do
              f <- readFile inp
              let h = render <$> encode (parse' f)
              writeFile out (fromMaybe "" h)
      