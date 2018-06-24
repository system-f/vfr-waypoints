{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aviation.VFR_Waypoints where

import Control.Category(Category((.), id))
import Control.Lens(Rewrapped, Wrapped(..), Cons(_Cons), Snoc(_Snoc), Each(each), Ixed(ix), Index, IxValue, AsEmpty(_Empty), Reversing(reversing), Lens', iso, prism', cons, snoc, _1, _2, (%~), _Wrapped, ( # ), (^?))
import Data.Eq(Eq)
import Data.Functor(fmap, (<$>))
import Data.Int(Int)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Traversable
import Prelude(Double, Show)

data Lat =
  Lat
    Int
    Double
  deriving (Eq, Ord, Show)

class HasLat c_aC1P where
  latitude :: Lens' c_aC1P Lat
  latitudeExponent :: Lens' c_aC1P Int
  {-# INLINE latitudeExponent #-}
  latitudeMantissa :: Lens' c_aC1P Double
  {-# INLINE latitudeMantissa #-}
  latitudeExponent = latitude . latitudeExponent
  latitudeMantissa = latitude . latitudeMantissa
instance HasLat Lat where
  {-# INLINE latitudeExponent #-}
  {-# INLINE latitudeMantissa #-}
  latitude = id
  latitudeExponent f_aC1Q (Lat x1_aC1R x2_aC1S)
    = (fmap (\ y1_aC1T -> (Lat y1_aC1T) x2_aC1S)) (f_aC1Q x1_aC1R)
  latitudeMantissa f_aC1U (Lat x1_aC1V x2_aC1W)
    = (fmap (\ y1_aC1X -> (Lat x1_aC1V) y1_aC1X)) (f_aC1U x2_aC1W)

data Lon =
  Lon
    Int
    Double
  deriving (Eq, Ord, Show)

class HasLon c_aC1P where
  longitude :: Lens' c_aC1P Lon
  longitudeExponent :: Lens' c_aC1P Int
  {-# INLINE longitudeExponent #-}
  longitudeMantissa :: Lens' c_aC1P Double
  {-# INLINE longitudeMantissa #-}
  longitudeExponent = longitude . longitudeExponent
  longitudeMantissa = longitude . longitudeMantissa
instance HasLon Lon where
  {-# INLINE longitudeExponent #-}
  {-# INLINE longitudeMantissa #-}
  longitude = id
  longitudeExponent f_aC1Q (Lon x1_aC1R x2_aC1S)
    = (fmap (\ y1_aC1T -> (Lon y1_aC1T) x2_aC1S)) (f_aC1Q x1_aC1R)
  longitudeMantissa f_aC1U (Lon x1_aC1V x2_aC1W)
    = (fmap (\ y1_aC1X -> (Lon x1_aC1V) y1_aC1X)) (f_aC1U x2_aC1W)

data VFR_Waypoint =
  VFR_Waypoint
    String
    (Maybe String)
    String
    Lat
    Lon
  deriving (Eq, Ord, Show)

instance HasLat VFR_Waypoint where
  latitude =
    lat . latitude

instance HasLon VFR_Waypoint where
  longitude =
    lon . longitude

class HasVFR_Waypoint a where
  vfr_waypoint ::
    Lens' a VFR_Waypoint

  name ::
    Lens' a String  
  name =
    vfr_waypoint . name
  {-# INLINE name #-}

  state ::
    Lens' a (Maybe String)
  state =
    vfr_waypoint . state
  {-# INLINE state #-}

  code ::
    Lens' a String
  code =
    vfr_waypoint . code
  {-# INLINE code #-}

  lat ::
    Lens' a Lat
  lat =
    vfr_waypoint . lat
  {-# INLINE lat #-}

  lon ::
    Lens' a Lon
  lon =
    vfr_waypoint . lon
  {-# INLINE lon #-}

instance HasVFR_Waypoint VFR_Waypoint where
  {-# INLINE name #-}
  {-# INLINE state #-}
  {-# INLINE code #-}
  {-# INLINE lat #-}
  {-# INLINE lon #-}
  vfr_waypoint =
    id

  name f_aq1f (VFR_Waypoint x1_aq1g x2_aq1h x3_aq1i x4_aq1j x5_aq1k) =
    (fmap
         (\ y1_aq1l
            -> ((((VFR_Waypoint y1_aq1l) x2_aq1h) x3_aq1i) x4_aq1j) x5_aq1k))
        (f_aq1f x1_aq1g)
  state f_aq1m (VFR_Waypoint x1_aq1n x2_aq1o x3_aq1p x4_aq1q x5_aq1r) =
    (fmap
         (\ y1_aq1s
            -> ((((VFR_Waypoint x1_aq1n) y1_aq1s) x3_aq1p) x4_aq1q) x5_aq1r))
        (f_aq1m x2_aq1o)
  code f_aq1t (VFR_Waypoint x1_aq1u x2_aq1v x3_aq1w x4_aq1x x5_aq1y) =
    (fmap
         (\ y1_aq1z
            -> ((((VFR_Waypoint x1_aq1u) x2_aq1v) y1_aq1z) x4_aq1x) x5_aq1y))
        (f_aq1t x3_aq1w)
  lat f_aq1A (VFR_Waypoint x1_aq1B x2_aq1C x3_aq1D x4_aq1E x5_aq1F) =
    (fmap
         (\ y1_aq1G
            -> ((((VFR_Waypoint x1_aq1B) x2_aq1C) x3_aq1D) y1_aq1G) x5_aq1F))
        (f_aq1A x4_aq1E)
  lon f_aq1H (VFR_Waypoint x1_aq1I x2_aq1J x3_aq1K x4_aq1L x5_aq1M) =
    (fmap
         (\ y1_aq1N
            -> ((((VFR_Waypoint x1_aq1I) x2_aq1J) x3_aq1K) x4_aq1L) y1_aq1N))
        (f_aq1H x5_aq1M)

newtype VFR_Waypoints =
  VFR_Waypoints
    [VFR_Waypoint]
  deriving (Eq, Ord, Show)

instance VFR_Waypoints ~ x => Rewrapped VFR_Waypoints x

instance Wrapped VFR_Waypoints where
  type Unwrapped VFR_Waypoints =
    [VFR_Waypoint]
  _Wrapped' =
    iso (\(VFR_Waypoints x) -> x) VFR_Waypoints

instance Cons VFR_Waypoints VFR_Waypoints VFR_Waypoint VFR_Waypoint where
  _Cons =
    prism'
      (\(x, VFR_Waypoints xs) -> VFR_Waypoints (x `cons` xs))
      (\(VFR_Waypoints x) -> fmap (_2 %~ (_Wrapped #)) (x ^? _Cons))
      
instance Snoc VFR_Waypoints VFR_Waypoints VFR_Waypoint VFR_Waypoint where
  _Snoc =
    prism'
      (\(VFR_Waypoints xs, x) -> VFR_Waypoints (xs `snoc` x))
      (\(VFR_Waypoints x) -> fmap (_1 %~ (_Wrapped #)) (x ^? _Snoc))

instance Each VFR_Waypoints VFR_Waypoints VFR_Waypoint VFR_Waypoint where
  each f (VFR_Waypoints x) =
    VFR_Waypoints <$> traverse f x

type instance Index VFR_Waypoints = Int
type instance IxValue VFR_Waypoints = VFR_Waypoint

instance Ixed VFR_Waypoints where
  ix n k (VFR_Waypoints x) =
    VFR_Waypoints <$> ix n k x

instance Semigroup VFR_Waypoints where
  VFR_Waypoints x <> VFR_Waypoints y =
    VFR_Waypoints (x <> y)

instance Monoid VFR_Waypoints where
  mappend =
    (<>)
  mempty =
    VFR_Waypoints []

instance AsEmpty VFR_Waypoints where
  _Empty =
    prism'
      (\() -> VFR_Waypoints [])
      (\(VFR_Waypoints x) -> case x of
                                [] ->
                                  Just ()
                                _:_ ->
                                  Nothing)

instance Reversing VFR_Waypoints where
  reversing (VFR_Waypoints x) =
    VFR_Waypoints (reversing x)

---- generated

_KNO_ ::
  VFR_Waypoint
_KNO_ =
  VFR_Waypoint
    "ABEAM KUNOTH"
    (Just "NT")
    "KNO"
    (Lat (-23) 32.5)
    (Lon 133 33.0)

_ABKL_ ::
  VFR_Waypoint
_ABKL_ =
  VFR_Waypoint
    "ABM KILTO"
    (Just "WA")
    "ABKL"
    (Lat (-17) 44.3)
    (Lon 122 44.1)

_TVT_ ::
  VFR_Waypoint
_TVT_ =
  VFR_Waypoint
    "ABM TV TOWERS"
    (Just "QLD")
    "TVT"
    (Lat (-27) 28.5)
    (Lon 152 55.0)

_ACE_ ::
  VFR_Waypoint
_ACE_ =
  VFR_Waypoint
    "ACADEMY"
    (Just "VIC")
    "ACE"
    (Lat (-37) 53.8)
    (Lon 145 10.8)

_ANI_ ::
  VFR_Waypoint
_ANI_ =
  VFR_Waypoint
    "ACHERON ISLAND"
    (Just "QLD")
    "ANI"
    (Lat (-18) 57.7)
    (Lon 146 38.2)

_ACLD_ ::
  VFR_Waypoint
_ACLD_ =
  VFR_Waypoint
    "ACLAND"
    (Just "QLD")
    "ACLD"
    (Lat (-27) 18.3)
    (Lon 151 41.3)

_ACTY_ ::
  VFR_Waypoint
_ACTY_ =
  VFR_Waypoint
    "ADELAIDE CBD"
    (Just "SA")
    "ACTY"
    (Lat (-34) 56.0)
    (Lon 138 36.0)

_ADB_ ::
  VFR_Waypoint
_ADB_ =
  VFR_Waypoint
    "ADELAIDE RIVER BRIDGE"
    (Just "NT")
    "ADB"
    (Lat (-12) 39.5)
    (Lon 131 20.0)

_AOG_ ::
  VFR_Waypoint
_AOG_ =
  VFR_Waypoint
    "ADELONG"
    (Just "NSW")
    "AOG"
    (Lat (-35) 18.5)
    (Lon 148 4.0)

_ADI_ ::
  VFR_Waypoint
_ADI_ =
  VFR_Waypoint
    "ADMIRALTY ISLAND"
    (Just "QLD")
    "ADI"
    (Lat (-16) 59.0)
    (Lon 145 46.5)

_ADWD_ ::
  VFR_Waypoint
_ADWD_ =
  VFR_Waypoint
    "ADVENTURE WORLD"
    (Just "WA")
    "ADWD"
    (Lat (-32) 5.7)
    (Lon 115 49.1)

_ALON_ ::
  VFR_Waypoint
_ALON_ =
  VFR_Waypoint
    "ALAWOONA"
    (Just "SA")
    "ALON"
    (Lat (-34) 45.0)
    (Lon 140 30.0)

_APL_ ::
  VFR_Waypoint
_APL_ =
  VFR_Waypoint
    "ALBERT PARK LAKE"
    (Just "VIC")
    "APL"
    (Lat (-37) 51.3)
    (Lon 144 58.5)

_AKW_ ::
  VFR_Waypoint
_AKW_ =
  VFR_Waypoint
    "ALKIMOS WRECK"
    (Just "WA")
    "AKW"
    (Lat (-31) 36.5)
    (Lon 115 39.0)

_ALBA_ ::
  VFR_Waypoint
_ALBA_ =
  VFR_Waypoint
    "ALOOMBA"
    (Just "QLD")
    "ALBA"
    (Lat (-17) 6.3)
    (Lon 145 50.0)

_ALOA_ ::
  VFR_Waypoint
_ALOA_ =
  VFR_Waypoint
    "ALTONA"
    (Just "VIC")
    "ALOA"
    (Lat (-37) 52.0)
    (Lon 144 51.0)

_ALTS_ ::
  VFR_Waypoint
_ALTS_ =
  VFR_Waypoint
    "ALTONA SOUTH"
    (Just "VIC")
    "ALTS"
    (Lat (-37) 52.7)
    (Lon 144 48.6)

_ANG_ ::
  VFR_Waypoint
_ANG_ =
  VFR_Waypoint
    "ANGLESEA"
    (Just "VIC")
    "ANG"
    (Lat (-38) 25.0)
    (Lon 144 11.0)

_APM_ ::
  VFR_Waypoint
_APM_ =
  VFR_Waypoint
    "ANM PAPER MILL"
    (Just "NSW")
    "APM"
    (Lat (-36) 0.0)
    (Lon 146 59.0)

_ANA_ ::
  VFR_Waypoint
_ANA_ =
  VFR_Waypoint
    "ANNA BAY"
    (Just "NSW")
    "ANA"
    (Lat (-32) 47.0)
    (Lon 152 5.0)

_ANP_ ::
  VFR_Waypoint
_ANP_ =
  VFR_Waypoint
    "ANTILL PLAINS"
    (Just "QLD")
    "ANP"
    (Lat (-19) 26.0)
    (Lon 146 50.0)

_APPN_ ::
  VFR_Waypoint
_APPN_ =
  VFR_Waypoint
    "APPIN"
    (Just "NSW")
    "APPN"
    (Lat (-34) 12.0)
    (Lon 150 47.3)

_ARCD_ ::
  VFR_Waypoint
_ARCD_ =
  VFR_Waypoint
    "ARCADIA HS"
    (Just "QLD")
    "ARCD"
    (Lat (-20) 52.0)
    (Lon 138 4.0)

_AEN_ ::
  VFR_Waypoint
_AEN_ =
  VFR_Waypoint
    "ARDENT"
    (Just "QLD")
    "AEN"
    (Lat (-26) 46.7)
    (Lon 152 34.7)

_ARE_ ::
  VFR_Waypoint
_ARE_ =
  VFR_Waypoint
    "ARMADALE"
    (Just "WA")
    "ARE"
    (Lat (-32) 8.6)
    (Lon 116 0.8)

_AWP_ ::
  VFR_Waypoint
_AWP_ =
  VFR_Waypoint
    "ARROWSMITH PT"
    (Just "NT")
    "AWP"
    (Lat (-13) 15.0)
    (Lon 136 27.0)

_ASU_ ::
  VFR_Waypoint
_ASU_ =
  VFR_Waypoint
    "ARUNDEL SUBSTATION"
    (Just "NSW")
    "ASU"
    (Lat (-35) 12.5)
    (Lon 147 24.0)

_ATN_ ::
  VFR_Waypoint
_ATN_ =
  VFR_Waypoint
    "ATHERTON"
    (Just "QLD")
    "ATN"
    (Lat (-17) 15.5)
    (Lon 145 30.5)

_ATG_ ::
  VFR_Waypoint
_ATG_ =
  VFR_Waypoint
    "ATTUNGA"
    (Just "NSW")
    "ATG"
    (Lat (-30) 56.0)
    (Lon 150 50.3)

_ATV_ ::
  VFR_Waypoint
_ATV_ =
  VFR_Waypoint
    "ATV10"
    (Just "VIC")
    "ATV"
    (Lat (-37) 51.2)
    (Lon 145 10.1)

_ALEC_ ::
  VFR_Waypoint
_ALEC_ =
  VFR_Waypoint
    "AUSTRALIAN LIVESTOCK EQUINE CENT"
    (Just "NSW")
    "ALEC"
    (Lat (-31) 8.1)
    (Lon 150 55.3)

_AVCA_ ::
  VFR_Waypoint
_AVCA_ =
  VFR_Waypoint
    "AVOCA"
    (Just "TAS")
    "AVCA"
    (Lat (-41) 46.9)
    (Lon 147 43.2)

_BADA_ ::
  VFR_Waypoint
_BADA_ =
  VFR_Waypoint
    "BABINDA"
    (Just "QLD")
    "BADA"
    (Lat (-17) 20.5)
    (Lon 145 55.5)

_BMP_ ::
  VFR_Waypoint
_BMP_ =
  VFR_Waypoint
    "BACCHUS MARSH TOWNSHIP"
    (Just "VIC")
    "BMP"
    (Lat (-37) 40.5)
    (Lon 144 26.3)

_BKRL_ ::
  VFR_Waypoint
_BKRL_ =
  VFR_Waypoint
    "BAKER LAKE"
    (Just "WA")
    "BKRL"
    (Lat (-26) 42.7)
    (Lon 125 58.8)

_BLHS_ ::
  VFR_Waypoint
_BLHS_ =
  VFR_Waypoint
    "BALD HILLS MAST"
    (Just "QLD")
    "BLHS"
    (Lat (-27) 18.8)
    (Lon 153 1.0)

_BOA_ ::
  VFR_Waypoint
_BOA_ =
  VFR_Waypoint
    "BALMORAL"
    (Just "VIC")
    "BOA"
    (Lat (-37) 15.0)
    (Lon 141 50.0)

_BANG_ ::
  VFR_Waypoint
_BANG_ =
  VFR_Waypoint
    "BANGALOW"
    (Just "NSW")
    "BANG"
    (Lat (-28) 41.2)
    (Lon 153 31.0)

_BDT_ ::
  VFR_Waypoint
_BDT_ =
  VFR_Waypoint
    "BARANDUDA TOWERS"
    (Just "VIC")
    "BDT"
    (Lat (-36) 15.0)
    (Lon 146 51.0)

_BARB_ ::
  VFR_Waypoint
_BARB_ =
  VFR_Waypoint
    "BARBAGALLO RACEWAY"
    (Just "WA")
    "BARB"
    (Lat (-31) 40.0)
    (Lon 115 47.0)

_BHCP_ ::
  VFR_Waypoint
_BHCP_ =
  VFR_Waypoint
    "BARN HILL CARAVAN PARK"
    (Just "WA")
    "BHCP"
    (Lat (-18) 22.1)
    (Lon 122 2.4)

_BSL_ ::
  VFR_Waypoint
_BSL_ =
  VFR_Waypoint
    "BARNES HILL"
    (Just "QLD")
    "BSL"
    (Lat (-27) 12.0)
    (Lon 152 6.0)

_BRNJ_ ::
  VFR_Waypoint
_BRNJ_ =
  VFR_Waypoint
    "BARRENJOEY HEAD"
    (Just "NSW")
    "BRNJ"
    (Lat (-33) 34.8)
    (Lon 151 19.6)

_BRR_ ::
  VFR_Waypoint
_BRR_ =
  VFR_Waypoint
    "BARRINGUN"
    (Just "NSW")
    "BRR"
    (Lat (-29) 1.0)
    (Lon 145 42.0)

_BRGE_ ::
  VFR_Waypoint
_BRGE_ =
  VFR_Waypoint
    "BARRON GORGE"
    (Just "QLD")
    "BRGE"
    (Lat (-16) 51.0)
    (Lon 145 39.0)

_BPN_ ::
  VFR_Waypoint
_BPN_ =
  VFR_Waypoint
    "BARWON PRISON"
    (Just "VIC")
    "BPN"
    (Lat (-37) 59.0)
    (Lon 144 21.0)

_KALK_ ::
  VFR_Waypoint
_KALK_ =
  VFR_Waypoint
    "BASS STRAIT"
    (Just "TAS")
    "KALK"
    (Lat (-39) 30.0)
    (Lon 142 30.0)

_BATB_ ::
  VFR_Waypoint
_BATB_ =
  VFR_Waypoint
    "BATEMANS BAY"
    (Just "NSW")
    "BATB"
    (Lat (-35) 43.0)
    (Lon 150 11.0)

_BTI_ ::
  VFR_Waypoint
_BTI_ =
  VFR_Waypoint
    "BATHURST ISLAND"
    (Just "NT")
    "BTI"
    (Lat (-11) 46.0)
    (Lon 130 37.0)

_BNE_ ::
  VFR_Waypoint
_BNE_ =
  VFR_Waypoint
    "BATMAN BRIDGE"
    (Just "TAS")
    "BNE"
    (Lat (-41) 13.1)
    (Lon 146 54.9)

_BAW_ ::
  VFR_Waypoint
_BAW_ =
  VFR_Waypoint
    "BAYWEST"
    (Just "VIC")
    "BAW"
    (Lat (-38) 0.0)
    (Lon 144 55.6)

_BCM_ ::
  VFR_Waypoint
_BCM_ =
  VFR_Waypoint
    "BEACHMERE"
    (Just "QLD")
    "BCM"
    (Lat (-27) 7.0)
    (Lon 153 3.0)

_BIV_ ::
  VFR_Waypoint
_BIV_ =
  VFR_Waypoint
    "BECTIVE HS"
    (Just "NSW")
    "BIV"
    (Lat (-30) 58.3)
    (Lon 150 44.3)

_BENT_ ::
  VFR_Waypoint
_BENT_ =
  VFR_Waypoint
    "BEECHMONT"
    (Just "QLD")
    "BENT"
    (Lat (-28) 8.0)
    (Lon 153 12.0)

_BCH_ ::
  VFR_Waypoint
_BCH_ =
  VFR_Waypoint
    "BEECHWORTH"
    (Just "VIC")
    "BCH"
    (Lat (-36) 21.5)
    (Lon 146 41.3)

_BLR_ ::
  VFR_Waypoint
_BLR_ =
  VFR_Waypoint
    "BELLBROOK"
    (Just "NSW")
    "BLR"
    (Lat (-30) 49.3)
    (Lon 152 30.7)

_BLIG_ ::
  VFR_Waypoint
_BLIG_ =
  VFR_Waypoint
    "BELLINGEN"
    (Just "NSW")
    "BLIG"
    (Lat (-30) 27.3)
    (Lon 152 53.8)

_BENV_ ::
  VFR_Waypoint
_BENV_ =
  VFR_Waypoint
    "BEN NEVIS"
    (Just "TAS")
    "BENV"
    (Lat (-41) 24.7)
    (Lon 147 38.5)

_BND_ ::
  VFR_Waypoint
_BND_ =
  VFR_Waypoint
    "BENDEMEER"
    (Just "NSW")
    "BND"
    (Lat (-30) 53.0)
    (Lon 151 9.0)

_BEE_ ::
  VFR_Waypoint
_BEE_ =
  VFR_Waypoint
    "BERILEE"
    (Just "NSW")
    "BEE"
    (Lat (-33) 37.2)
    (Lon 151 6.3)

_BESI_ ::
  VFR_Waypoint
_BESI_ =
  VFR_Waypoint
    "BESSIE POINT"
    (Just "QLD")
    "BESI"
    (Lat (-16) 54.2)
    (Lon 145 48.8)

_BVG_ ::
  VFR_Waypoint
_BVG_ =
  VFR_Waypoint
    "BEVERIDGE"
    (Just "VIC")
    "BVG"
    (Lat (-37) 28.3)
    (Lon 144 58.3)

_BDTN_ ::
  VFR_Waypoint
_BDTN_ =
  VFR_Waypoint
    "BIDDESTON"
    (Just "QLD")
    "BDTN"
    (Lat (-27) 33.5)
    (Lon 151 43.0)

_BIBA_ ::
  VFR_Waypoint
_BIBA_ =
  VFR_Waypoint
    "BINGIL BAY"
    (Just "QLD")
    "BIBA"
    (Lat (-17) 49.7)
    (Lon 146 6.1)

_BDWD_ ::
  VFR_Waypoint
_BDWD_ =
  VFR_Waypoint
    "BIRDWOOD"
    (Just "SA")
    "BDWD"
    (Lat (-34) 49.4)
    (Lon 138 57.6)

_BCT_ ::
  VFR_Waypoint
_BCT_ =
  VFR_Waypoint
    "BLACK MT"
    (Just "QLD")
    "BCT"
    (Lat (-21) 4.2)
    (Lon 149 5.8)

_BKM_ ::
  VFR_Waypoint
_BKM_ =
  VFR_Waypoint
    "BLACK MT"
    (Just "ACT")
    "BKM"
    (Lat (-35) 16.5)
    (Lon 149 6.0)

_BKIS_ ::
  VFR_Waypoint
_BKIS_ =
  VFR_Waypoint
    "BLACKSMITH ISLAND"
    (Just "QLD")
    "BKIS"
    (Lat (-20) 38.0)
    (Lon 149 4.0)

_BLIC_ ::
  VFR_Waypoint
_BLIC_ =
  VFR_Waypoint
    "BLI BLI CASTLE"
    (Just "QLD")
    "BLIC"
    (Lat (-26) 37.5)
    (Lon 153 2.0)

_BTM_ ::
  VFR_Waypoint
_BTM_ =
  VFR_Waypoint
    "BOAT HARBOUR"
    (Just "TAS")
    "BTM"
    (Lat (-40) 57.0)
    (Lon 145 38.0)

_BOAT_ ::
  VFR_Waypoint
_BOAT_ =
  VFR_Waypoint
    "BOATYARD"
    (Just "WA")
    "BOAT"
    (Lat (-32) 9.0)
    (Lon 115 46.0)

_BODD_ ::
  VFR_Waypoint
_BODD_ =
  VFR_Waypoint
    "BODDINGTON"
    (Just "WA")
    "BODD"
    (Lat (-32) 48.0)
    (Lon 116 28.0)

_BGN_ ::
  VFR_Waypoint
_BGN_ =
  VFR_Waypoint
    "BOGANTUNGAN"
    (Just "QLD")
    "BGN"
    (Lat (-23) 39.0)
    (Lon 147 18.0)

_BLTB_ ::
  VFR_Waypoint
_BLTB_ =
  VFR_Waypoint
    "BOLTE BRIDGE"
    (Just "VIC")
    "BLTB"
    (Lat (-37) 49.2)
    (Lon 144 55.9)

_BSP_ ::
  VFR_Waypoint
_BSP_ =
  VFR_Waypoint
    "BOND SPRINGS"
    (Just "NT")
    "BSP"
    (Lat (-23) 31.0)
    (Lon 133 51.0)

_BUVY_ ::
  VFR_Waypoint
_BUVY_ =
  VFR_Waypoint
    "BOND UNIVERSITY"
    (Just "QLD")
    "BUVY"
    (Lat (-28) 4.6)
    (Lon 153 24.6)

_BNG_ ::
  VFR_Waypoint
_BNG_ =
  VFR_Waypoint
    "BONEGILLA"
    (Just "VIC")
    "BNG"
    (Lat (-36) 8.7)
    (Lon 147 0.8)

_BONG_ ::
  VFR_Waypoint
_BONG_ =
  VFR_Waypoint
    "BONGAREE"
    (Just "QLD")
    "BONG"
    (Lat (-27) 5.0)
    (Lon 153 11.0)

_BOON_ ::
  VFR_Waypoint
_BOON_ =
  VFR_Waypoint
    "BOONDALL ENTERTAINMENT CENTRE"
    (Just "QLD")
    "BOON"
    (Lat (-27) 20.5)
    (Lon 153 5.1)

_BOAR_ ::
  VFR_Waypoint
_BOAR_ =
  VFR_Waypoint
    "BORUMBA RESV"
    (Just "QLD")
    "BOAR"
    (Lat (-26) 30.5)
    (Lon 152 35.0)

_BBH_ ::
  VFR_Waypoint
_BBH_ =
  VFR_Waypoint
    "BOTANY BAY HEADS"
    (Just "NSW")
    "BBH"
    (Lat (-34) 0.5)
    (Lon 151 14.5)

_BOWEN_ ::
  VFR_Waypoint
_BOWEN_ =
  VFR_Waypoint
    "BOWEN"
    (Just "QLD")
    "BOWEN"
    (Lat (-20) 1.0)
    (Lon 148 14.5)

_BOWB_ ::
  VFR_Waypoint
_BOWB_ =
  VFR_Waypoint
    "BOWEN BRIDGE"
    (Just "TAS")
    "BOWB"
    (Lat (-42) 49.0)
    (Lon 147 18.0)

_BWV_ ::
  VFR_Waypoint
_BWV_ =
  VFR_Waypoint
    "BOWENVILLE"
    (Just "QLD")
    "BWV"
    (Lat (-27) 18.3)
    (Lon 151 29.3)

_BWMS_ ::
  VFR_Waypoint
_BWMS_ =
  VFR_Waypoint
    "BOWMANS"
    (Just "SA")
    "BWMS"
    (Lat (-34) 9.0)
    (Lon 138 16.0)

_BOW_ ::
  VFR_Waypoint
_BOW_ =
  VFR_Waypoint
    "BOWNA"
    (Just "NSW")
    "BOW"
    (Lat (-35) 57.0)
    (Lon 147 7.0)

_BWL_ ::
  VFR_Waypoint
_BWL_ =
  VFR_Waypoint
    "BOWRAL"
    (Just "NSW")
    "BWL"
    (Lat (-34) 28.8)
    (Lon 150 25.1)

_BOV_ ::
  VFR_Waypoint
_BOV_ =
  VFR_Waypoint
    "BOWRAVILLE"
    (Just "NSW")
    "BOV"
    (Lat (-30) 39.0)
    (Lon 152 51.0)

_BPI_ ::
  VFR_Waypoint
_BPI_ =
  VFR_Waypoint
    "BRAMPTON ISLAND"
    (Just "QLD")
    "BPI"
    (Lat (-20) 48.3)
    (Lon 149 16.0)

_BAXT_ ::
  VFR_Waypoint
_BAXT_ =
  VFR_Waypoint
    "BRANXTON"
    (Just "NSW")
    "BAXT"
    (Lat (-32) 40.0)
    (Lon 151 21.0)

_BZA_ ::
  VFR_Waypoint
_BZA_ =
  VFR_Waypoint
    "BREEZA"
    (Just "NSW")
    "BZA"
    (Lat (-31) 15.0)
    (Lon 150 28.0)

_BBBG_ ::
  VFR_Waypoint
_BBBG_ =
  VFR_Waypoint
    "BRIBIE BRIDGE"
    (Just "QLD")
    "BBBG"
    (Lat (-27) 4.4)
    (Lon 153 8.8)

_BBI_ ::
  VFR_Waypoint
_BBI_ =
  VFR_Waypoint
    "BRIBIE ISLAND"
    (Just "QLD")
    "BBI"
    (Lat (-27) 0.0)
    (Lon 153 8.6)

_BTO_ ::
  VFR_Waypoint
_BTO_ =
  VFR_Waypoint
    "BRIGHTON"
    (Just "VIC")
    "BTO"
    (Lat (-37) 54.7)
    (Lon 144 59.2)

_BTJ_ ::
  VFR_Waypoint
_BTJ_ =
  VFR_Waypoint
    "BRIGHTON JETTY"
    (Just "SA")
    "BTJ"
    (Lat (-35) 1.0)
    (Lon 138 31.0)

_BRY_ ::
  VFR_Waypoint
_BRY_ =
  VFR_Waypoint
    "BRINGELLY"
    (Just "NSW")
    "BRY"
    (Lat (-33) 56.5)
    (Lon 150 43.7)

_BRIN_ ::
  VFR_Waypoint
_BRIN_ =
  VFR_Waypoint
    "BRINSMEAD"
    (Just "QLD")
    "BRIN"
    (Lat (-16) 54.5)
    (Lon 145 42.5)

_BCTY_ ::
  VFR_Waypoint
_BCTY_ =
  VFR_Waypoint
    "BRISBANE CBD"
    (Just "QLD")
    "BCTY"
    (Lat (-27) 28.0)
    (Lon 153 2.0)

_OGABA_ ::
  VFR_Waypoint
_OGABA_ =
  VFR_Waypoint
    "BRISBANE CRICKET GROUND"
    (Just "QLD")
    "OGABA"
    (Lat (-27) 28.8)
    (Lon 153 4.0)

_BDF_ ::
  VFR_Waypoint
_BDF_ =
  VFR_Waypoint
    "BROADFORD"
    (Just "VIC")
    "BDF"
    (Lat (-37) 12.5)
    (Lon 145 2.5)

_BYN_ ::
  VFR_Waypoint
_BYN_ =
  VFR_Waypoint
    "BROOKLYN"
    (Just "VIC")
    "BYN"
    (Lat (-37) 49.7)
    (Lon 144 51.5)

_BBG_ ::
  VFR_Waypoint
_BBG_ =
  VFR_Waypoint
    "BROOKLYN BRIDGE"
    (Just "NSW")
    "BBG"
    (Lat (-33) 32.5)
    (Lon 151 11.8)

_BTON_ ::
  VFR_Waypoint
_BTON_ =
  VFR_Waypoint
    "BROOKTON"
    (Just "WA")
    "BTON"
    (Lat (-32) 22.0)
    (Lon 117 1.0)

_BRI_ ::
  VFR_Waypoint
_BRI_ =
  VFR_Waypoint
    "BROUGHTON ISLAND"
    (Just "NSW")
    "BRI"
    (Lat (-32) 36.5)
    (Lon 152 18.5)

_OBSTM_ ::
  VFR_Waypoint
_OBSTM_ =
  VFR_Waypoint
    "BRUCE STADIUM"
    (Just "ACT")
    "OBSTM"
    (Lat (-35) 15.0)
    (Lon 149 6.0)

_BRH_ ::
  VFR_Waypoint
_BRH_ =
  VFR_Waypoint
    "BRUNSWICK HEADS"
    (Just "NSW")
    "BRH"
    (Lat (-28) 32.5)
    (Lon 153 33.0)

_BUCP_ ::
  VFR_Waypoint
_BUCP_ =
  VFR_Waypoint
    "BUCHAN PT"
    (Just "QLD")
    "BUCP"
    (Lat (-16) 44.2)
    (Lon 145 40.1)

_BUCN_ ::
  VFR_Waypoint
_BUCN_ =
  VFR_Waypoint
    "BUCHANAN HILLS"
    (Just "NT")
    "BUCN"
    (Lat (-18) 54.0)
    (Lon 131 5.0)

_BKD_ ::
  VFR_Waypoint
_BKD_ =
  VFR_Waypoint
    "BUCKLAND"
    (Just "TAS")
    "BKD"
    (Lat (-42) 36.7)
    (Lon 147 43.0)

_BPK_ ::
  VFR_Waypoint
_BPK_ =
  VFR_Waypoint
    "BUCKLAND PARK WEATHER RADAR"
    (Just "SA")
    "BPK"
    (Lat (-34) 37.0)
    (Lon 138 28.1)

_BDH_ ::
  VFR_Waypoint
_BDH_ =
  VFR_Waypoint
    "BUNDAGEN HEAD"
    (Just "NSW")
    "BDH"
    (Lat (-30) 26.0)
    (Lon 153 4.5)

_BEN_ ::
  VFR_Waypoint
_BEN_ =
  VFR_Waypoint
    "BUNGENDORE"
    (Just "NSW")
    "BEN"
    (Lat (-35) 15.5)
    (Lon 149 26.8)

_BUG_ ::
  VFR_Waypoint
_BUG_ =
  VFR_Waypoint
    "BUNGIL BRIDGE"
    (Just "VIC")
    "BUG"
    (Lat (-36) 3.0)
    (Lon 147 21.0)

_URG_ ::
  VFR_Waypoint
_URG_ =
  VFR_Waypoint
    "BURBONG"
    (Just "NSW")
    "URG"
    (Lat (-35) 20.4)
    (Lon 149 18.6)

_BLGH_ ::
  VFR_Waypoint
_BLGH_ =
  VFR_Waypoint
    "BURLEIGH HEADS"
    (Just "QLD")
    "BLGH"
    (Lat (-28) 5.5)
    (Lon 153 27.5)

_BUB_ ::
  VFR_Waypoint
_BUB_ =
  VFR_Waypoint
    "BURNS BEACH"
    (Just "WA")
    "BUB"
    (Lat (-31) 43.7)
    (Lon 115 43.0)

_BURR_ ::
  VFR_Waypoint
_BURR_ =
  VFR_Waypoint
    "BURRA"
    (Just "SA")
    "BURR"
    (Lat (-33) 41.0)
    (Lon 138 56.0)

_BUGA_ ::
  VFR_Waypoint
_BUGA_ =
  VFR_Waypoint
    "BURRAGA"
    (Just "NSW")
    "BUGA"
    (Lat (-33) 53.0)
    (Lon 149 34.0)

_BJK_ ::
  VFR_Waypoint
_BJK_ =
  VFR_Waypoint
    "BURRINJUCK"
    (Just "NSW")
    "BJK"
    (Lat (-35) 0.3)
    (Lon 148 35.0)

_BMK_ ::
  VFR_Waypoint
_BMK_ =
  VFR_Waypoint
    "BURRUMBUTTOCK"
    (Just "NSW")
    "BMK"
    (Lat (-35) 50.0)
    (Lon 146 48.0)

_BYFD_ ::
  VFR_Waypoint
_BYFD_ =
  VFR_Waypoint
    "BYFORD"
    (Just "WA")
    "BYFD"
    (Lat (-32) 13.0)
    (Lon 116 2.0)

_BYNO_ ::
  VFR_Waypoint
_BYNO_ =
  VFR_Waypoint
    "BYNOE HARBOUR"
    (Just "NT")
    "BYNO"
    (Lat (-12) 45.0)
    (Lon 130 41.0)

_BYRK_ ::
  VFR_Waypoint
_BYRK_ =
  VFR_Waypoint
    "BYROCK"
    (Just "NSW")
    "BYRK"
    (Lat (-30) 39.0)
    (Lon 146 24.0)

_BBAY_ ::
  VFR_Waypoint
_BBAY_ =
  VFR_Waypoint
    "BYRON BAY"
    (Just "NSW")
    "BBAY"
    (Lat (-28) 39.0)
    (Lon 153 37.0)

_CBLT_ ::
  VFR_Waypoint
_CBLT_ =
  VFR_Waypoint
    "CABLEWAY TERMINAL"
    (Just "QLD")
    "CBLT"
    (Lat (-16) 50.9)
    (Lon 145 41.5)

_CABO_ ::
  VFR_Waypoint
_CABO_ =
  VFR_Waypoint
    "CABOOLTURE"
    (Just "QLD")
    "CABO"
    (Lat (-27) 5.3)
    (Lon 152 57.0)

_CALEN_ ::
  VFR_Waypoint
_CALEN_ =
  VFR_Waypoint
    "CALEN"
    (Just "QLD")
    "CALEN"
    (Lat (-20) 54.0)
    (Lon 148 46.2)

_CRDZ_ ::
  VFR_Waypoint
_CRDZ_ =
  VFR_Waypoint
    "CALOUNDRA DROPZONE"
    (Just "QLD")
    "CRDZ"
    (Lat (-26) 48.0)
    (Lon 153 6.6)

_CALT_ ::
  VFR_Waypoint
_CALT_ =
  VFR_Waypoint
    "CALTEX REFINERY"
    (Just "QLD")
    "CALT"
    (Lat (-27) 24.9)
    (Lon 153 9.5)

_CBRA_ ::
  VFR_Waypoint
_CBRA_ =
  VFR_Waypoint
    "CAMBRAI"
    (Just "SA")
    "CBRA"
    (Lat (-34) 39.5)
    (Lon 139 16.8)

_CPA_ ::
  VFR_Waypoint
_CPA_ =
  VFR_Waypoint
    "CAMPANIA"
    (Just "TAS")
    "CPA"
    (Lat (-42) 40.0)
    (Lon 147 25.3)

_CLLN_ ::
  VFR_Waypoint
_CLLN_ =
  VFR_Waypoint
    "CAMPBELLTOWN"
    (Just "TAS")
    "CLLN"
    (Lat (-41) 56.0)
    (Lon 147 29.5)

_CAMB_ ::
  VFR_Waypoint
_CAMB_ =
  VFR_Waypoint
    "CAMPBELLTOWN UNIVERSITY"
    (Just "NSW")
    "CAMB"
    (Lat (-34) 4.3)
    (Lon 150 47.0)

_RCSE_ ::
  VFR_Waypoint
_RCSE_ =
  VFR_Waypoint
    "CANBERRA RACECOURSE"
    (Just "ACT")
    "RCSE"
    (Lat (-35) 14.3)
    (Lon 149 8.2)

_CNB_ ::
  VFR_Waypoint
_CNB_ =
  VFR_Waypoint
    "CANNING BRIDGE"
    (Just "WA")
    "CNB"
    (Lat (-32) 0.6)
    (Lon 115 51.0)

_CDM_ ::
  VFR_Waypoint
_CDM_ =
  VFR_Waypoint
    "CANNING DAM"
    (Just "WA")
    "CDM"
    (Lat (-32) 9.3)
    (Lon 116 7.5)

_CAV_ ::
  VFR_Waypoint
_CAV_ =
  VFR_Waypoint
    "CANNONVALE"
    (Just "QLD")
    "CAV"
    (Lat (-20) 16.7)
    (Lon 148 41.5)

_CBY_ ::
  VFR_Waypoint
_CBY_ =
  VFR_Waypoint
    "CANTERBURY RACECOURSE"
    (Just "NSW")
    "CBY"
    (Lat (-33) 54.5)
    (Lon 151 6.7)

_CAPS_ ::
  VFR_Waypoint
_CAPS_ =
  VFR_Waypoint
    "CAPE BANKS"
    (Just "NSW")
    "CAPS"
    (Lat (-33) 59.9)
    (Lon 151 14.7)

_CCL_ ::
  VFR_Waypoint
_CCL_ =
  VFR_Waypoint
    "CAPE CLEVELAND"
    (Just "QLD")
    "CCL"
    (Lat (-19) 11.0)
    (Lon 147 0.8)

_CPY_ ::
  VFR_Waypoint
_CPY_ =
  VFR_Waypoint
    "CAPE CONWAY"
    (Just "QLD")
    "CPY"
    (Lat (-20) 32.2)
    (Lon 148 55.7)

_CGR_ ::
  VFR_Waypoint
_CGR_ =
  VFR_Waypoint
    "CAPE GAMBIER"
    (Just "NT")
    "CGR"
    (Lat (-11) 56.3)
    (Lon 130 58.0)

_CGF_ ::
  VFR_Waypoint
_CGF_ =
  VFR_Waypoint
    "CAPE GRAFTON"
    (Just "QLD")
    "CGF"
    (Lat (-16) 51.8)
    (Lon 145 55.0)

_CPH_ ::
  VFR_Waypoint
_CPH_ =
  VFR_Waypoint
    "CAPE HILLSBOROUGH"
    (Just "QLD")
    "CPH"
    (Lat (-20) 54.3)
    (Lon 149 2.7)

_CAJE_ ::
  VFR_Waypoint
_CAJE_ =
  VFR_Waypoint
    "CAPE JERVIS"
    (Just "SA")
    "CAJE"
    (Lat (-35) 36.4)
    (Lon 138 5.5)

_CAKE_ ::
  VFR_Waypoint
_CAKE_ =
  VFR_Waypoint
    "CAPE KEITH"
    (Just "NT")
    "CAKE"
    (Lat (-11) 37.0)
    (Lon 131 28.0)

_CMB_ ::
  VFR_Waypoint
_CMB_ =
  VFR_Waypoint
    "CAPE LAMBERT"
    (Just "WA")
    "CMB"
    (Lat (-20) 35.6)
    (Lon 117 11.0)

_CPMN_ ::
  VFR_Waypoint
_CPMN_ =
  VFR_Waypoint
    "CAPE MORETON"
    (Just "QLD")
    "CPMN"
    (Lat (-27) 2.0)
    (Lon 153 28.0)

_CPLD_ ::
  VFR_Waypoint
_CPLD_ =
  VFR_Waypoint
    "CAPE PORTLAND"
    (Just "TAS")
    "CPLD"
    (Lat (-40) 45.0)
    (Lon 147 57.0)

_CPHE_ ::
  VFR_Waypoint
_CPHE_ =
  VFR_Waypoint
    "CAPE RICHE"
    (Just "WA")
    "CPHE"
    (Lat (-34) 36.0)
    (Lon 118 46.0)

_CAPT_ ::
  VFR_Waypoint
_CAPT_ =
  VFR_Waypoint
    "CAPTAINS FLAT"
    (Just "NSW")
    "CAPT"
    (Lat (-35) 35.5)
    (Lon 149 26.7)

_CAU_ ::
  VFR_Waypoint
_CAU_ =
  VFR_Waypoint
    "CARAMUT"
    (Just "VIC")
    "CAU"
    (Lat (-37) 58.0)
    (Lon 142 31.0)

_CARE_ ::
  VFR_Waypoint
_CARE_ =
  VFR_Waypoint
    "CARDINIA RESV"
    (Just "VIC")
    "CARE"
    (Lat (-37) 57.5)
    (Lon 145 25.0)

_CDNA_ ::
  VFR_Waypoint
_CDNA_ =
  VFR_Waypoint
    "CARDONA"
    (Just "QLD")
    "CDNA"
    (Lat (-23) 20.0)
    (Lon 149 0.0)

_CARIN_ ::
  VFR_Waypoint
_CARIN_ =
  VFR_Waypoint
    "CARINDALE SHOPPING CENTRE"
    (Just "QLD")
    "CARIN"
    (Lat (-27) 30.0)
    (Lon 153 6.1)

_CIK_ ::
  VFR_Waypoint
_CIK_ =
  VFR_Waypoint
    "CARRICK"
    (Just "TAS")
    "CIK"
    (Lat (-41) 32.0)
    (Lon 147 0.0)

_CARR_ ::
  VFR_Waypoint
_CARR_ =
  VFR_Waypoint
    "CARRUM"
    (Just "VIC")
    "CARR"
    (Lat (-38) 4.5)
    (Lon 145 7.2)

_CLJ_ ::
  VFR_Waypoint
_CLJ_ =
  VFR_Waypoint
    "CASTLE JUNCTION"
    (Just "TAS")
    "CLJ"
    (Lat (-41) 30.3)
    (Lon 147 29.0)

_CTT_ ::
  VFR_Waypoint
_CTT_ =
  VFR_Waypoint
    "CASTLE PT"
    (Just "NT")
    "CTT"
    (Lat (-12) 21.0)
    (Lon 131 16.5)

_CRPT_ ::
  VFR_Waypoint
_CRPT_ =
  VFR_Waypoint
    "CASTLEREAGH POINT"
    (Just "QLD")
    "CRPT"
    (Lat (-27) 11.5)
    (Lon 153 6.7)

_CCP_ ::
  VFR_Waypoint
_CCP_ =
  VFR_Waypoint
    "CECIL PLAINS"
    (Just "QLD")
    "CCP"
    (Lat (-27) 32.0)
    (Lon 151 11.0)

_CAO_ ::
  VFR_Waypoint
_CAO_ =
  VFR_Waypoint
    "CEDUNA OBSERVATORY"
    (Just "SA")
    "CAO"
    (Lat (-31) 52.0)
    (Lon 133 48.0)

_CBRG_ ::
  VFR_Waypoint
_CBRG_ =
  VFR_Waypoint
    "CENTENARY BRIDGE"
    (Just "QLD")
    "CBRG"
    (Lat (-27) 31.7)
    (Lon 152 56.8)

_CERB_ ::
  VFR_Waypoint
_CERB_ =
  VFR_Waypoint
    "CERBERUS"
    (Just "VIC")
    "CERB"
    (Lat (-37) 58.0)
    (Lon 145 0.0)

_CYM_ ::
  VFR_Waypoint
_CYM_ =
  VFR_Waypoint
    "CHAFFEY DAM"
    (Just "NSW")
    "CYM"
    (Lat (-31) 20.5)
    (Lon 151 8.0)

_CHI_ ::
  VFR_Waypoint
_CHI_ =
  VFR_Waypoint
    "CHANNEL ISLAND"
    (Just "NT")
    "CHI"
    (Lat (-12) 33.0)
    (Lon 130 52.0)

_CHAP_ ::
  VFR_Waypoint
_CHAP_ =
  VFR_Waypoint
    "CHARLES PT"
    (Just "NT")
    "CHAP"
    (Lat (-12) 23.0)
    (Lon 130 37.1)

_CHAT_ ::
  VFR_Waypoint
_CHAT_ =
  VFR_Waypoint
    "CHATSWOOD CBD"
    (Just "NSW")
    "CHAT"
    (Lat (-33) 47.8)
    (Lon 151 11.1)

_CHN_ ::
  VFR_Waypoint
_CHN_ =
  VFR_Waypoint
    "CHILTERN"
    (Just "VIC")
    "CHN"
    (Lat (-36) 8.8)
    (Lon 146 36.4)

_CIB_ ::
  VFR_Waypoint
_CIB_ =
  VFR_Waypoint
    "CHIPBOARD FACTORY"
    (Just "NSW")
    "CIB"
    (Lat (-35) 4.3)
    (Lon 147 24.3)

_COY_ ::
  VFR_Waypoint
_COY_ =
  VFR_Waypoint
    "CHOCOLATE FACTORY"
    (Just "TAS")
    "COY"
    (Lat (-42) 48.0)
    (Lon 147 16.0)

_CNTH_ ::
  VFR_Waypoint
_CNTH_ =
  VFR_Waypoint
    "CHOPPERS NORTH"
    (Just "NSW")
    "CNTH"
    (Lat (-33) 52.9)
    (Lon 151 1.5)

_CSTH_ ::
  VFR_Waypoint
_CSTH_ =
  VFR_Waypoint
    "CHOPPERS SOUTH"
    (Just "NSW")
    "CSTH"
    (Lat (-33) 57.3)
    (Lon 150 57.9)

_CWST_ ::
  VFR_Waypoint
_CWST_ =
  VFR_Waypoint
    "CHOPPERS WEST"
    (Just "NSW")
    "CWST"
    (Lat (-33) 52.4)
    (Lon 151 0.2)

_CIH_ ::
  VFR_Waypoint
_CIH_ =
  VFR_Waypoint
    "CID HARBOUR"
    (Just "QLD")
    "CIH"
    (Lat (-20) 15.0)
    (Lon 148 56.0)

_CYB_ ::
  VFR_Waypoint
_CYB_ =
  VFR_Waypoint
    "CITY BEACH"
    (Just "WA")
    "CYB"
    (Lat (-31) 56.5)
    (Lon 115 45.0)

_CEN_ ::
  VFR_Waypoint
_CEN_ =
  VFR_Waypoint
    "CLEVEDON"
    (Just "QLD")
    "CEN"
    (Lat (-19) 23.8)
    (Lon 147 1.3)

_CVD_ ::
  VFR_Waypoint
_CVD_ =
  VFR_Waypoint
    "CLEVELAND"
    (Just "QLD")
    "CVD"
    (Lat (-27) 31.2)
    (Lon 153 17.0)

_CFI_ ::
  VFR_Waypoint
_CFI_ =
  VFR_Waypoint
    "CLIFFY ISLAND"
    (Just "VIC")
    "CFI"
    (Lat (-38) 57.1)
    (Lon 146 42.1)

_CLS_ ::
  VFR_Waypoint
_CLS_ =
  VFR_Waypoint
    "CLIFTON SPRINGS"
    (Just "VIC")
    "CLS"
    (Lat (-38) 9.0)
    (Lon 144 34.0)

_CGH_ ::
  VFR_Waypoint
_CGH_ =
  VFR_Waypoint
    "CLONAGH STN"
    (Just "QLD")
    "CGH"
    (Lat (-20) 8.0)
    (Lon 140 41.0)

_CLOY_ ::
  VFR_Waypoint
_CLOY_ =
  VFR_Waypoint
    "CLONEYS CREEK"
    (Just "QLD")
    "CLOY"
    (Lat (-20) 13.0)
    (Lon 142 36.0)

_COBA_ ::
  VFR_Waypoint
_COBA_ =
  VFR_Waypoint
    "COBAKI"
    (Just "NSW")
    "COBA"
    (Lat (-28) 11.0)
    (Lon 153 29.3)

_CKT_ ::
  VFR_Waypoint
_CKT_ =
  VFR_Waypoint
    "COCKATOO"
    (Just "VIC")
    "CKT"
    (Lat (-37) 56.3)
    (Lon 145 29.7)

_CBI_ ::
  VFR_Waypoint
_CBI_ =
  VFR_Waypoint
    "COLBINABBIN"
    (Just "VIC")
    "CBI"
    (Lat (-36) 35.0)
    (Lon 144 48.0)

_CGE_ ::
  VFR_Waypoint
_CGE_ =
  VFR_Waypoint
    "COLLINGULLIE"
    (Just "NSW")
    "CGE"
    (Lat (-35) 5.3)
    (Lon 147 7.3)

_CONG_ ::
  VFR_Waypoint
_CONG_ =
  VFR_Waypoint
    "COMERONG ISLAND"
    (Just "NSW")
    "CONG"
    (Lat (-34) 53.0)
    (Lon 150 44.0)

_CJN_ ::
  VFR_Waypoint
_CJN_ =
  VFR_Waypoint
    "CONARA JUNCTION"
    (Just "TAS")
    "CJN"
    (Lat (-41) 50.0)
    (Lon 147 26.0)

_CGM_ ::
  VFR_Waypoint
_CGM_ =
  VFR_Waypoint
    "CONDONG MILL"
    (Just "NSW")
    "CGM"
    (Lat (-28) 19.0)
    (Lon 153 26.0)

_CBYC_ ::
  VFR_Waypoint
_CBYC_ =
  VFR_Waypoint
    "COOBY CREEK RESV"
    (Just "QLD")
    "CBYC"
    (Lat (-27) 23.2)
    (Lon 151 56.3)

_CIS_ ::
  VFR_Waypoint
_CIS_ =
  VFR_Waypoint
    "COOK ISLAND"
    (Just "NSW")
    "CIS"
    (Lat (-28) 11.8)
    (Lon 153 34.7)

_CLMN_ ::
  VFR_Waypoint
_CLMN_ =
  VFR_Waypoint
    "COOLAMON"
    (Just "NSW")
    "CLMN"
    (Lat (-34) 49.0)
    (Lon 147 12.0)

_CMDR_ ::
  VFR_Waypoint
_CMDR_ =
  VFR_Waypoint
    "COOLMUNDA RESV"
    (Just "QLD")
    "CMDR"
    (Lat (-28) 27.0)
    (Lon 151 14.0)

_COOL_ ::
  VFR_Waypoint
_COOL_ =
  VFR_Waypoint
    "COOLUM HI-RISE"
    (Just "QLD")
    "COOL"
    (Lat (-26) 31.7)
    (Lon 153 5.2)

_CORO_ ::
  VFR_Waypoint
_CORO_ =
  VFR_Waypoint
    "COOROY"
    (Just "QLD")
    "CORO"
    (Lat (-26) 25.0)
    (Lon 152 54.5)

_CPL_ ::
  VFR_Waypoint
_CPL_ =
  VFR_Waypoint
    "COPPERLODE DAM"
    (Just "QLD")
    "CPL"
    (Lat (-16) 59.2)
    (Lon 145 40.3)

_CPNG_ ::
  VFR_Waypoint
_CPNG_ =
  VFR_Waypoint
    "COPPINS CROSSING"
    (Just "ACT")
    "CPNG"
    (Lat (-35) 17.3)
    (Lon 149 2.6)

_CVR_ ::
  VFR_Waypoint
_CVR_ =
  VFR_Waypoint
    "CORIN RESV"
    (Just "ACT")
    "CVR"
    (Lat (-35) 32.5)
    (Lon 148 50.0)

_COSS_ ::
  VFR_Waypoint
_COSS_ =
  VFR_Waypoint
    "COSSACK"
    (Just "WA")
    "COSS"
    (Lat (-20) 40.6)
    (Lon 117 11.4)

_CTE_ ::
  VFR_Waypoint
_CTE_ =
  VFR_Waypoint
    "COTTESLOE"
    (Just "WA")
    "CTE"
    (Lat (-31) 59.5)
    (Lon 115 45.0)

_COWI_ ::
  VFR_Waypoint
_COWI_ =
  VFR_Waypoint
    "COW ISLAND"
    (Just "QLD")
    "COWI"
    (Lat (-20) 25.4)
    (Lon 148 50.7)

_COWR_ ::
  VFR_Waypoint
_COWR_ =
  VFR_Waypoint
    "COWWARR"
    (Just "VIC")
    "COWR"
    (Lat (-38) 0.9)
    (Lon 146 41.6)

_CML_ ::
  VFR_Waypoint
_CML_ =
  VFR_Waypoint
    "CRADLE MOUNTAIN LODGE"
    (Just "TAS")
    "CML"
    (Lat (-41) 35.7)
    (Lon 145 55.7)

_CBV_ ::
  VFR_Waypoint
_CBV_ =
  VFR_Waypoint
    "CRAIGBOURNE RESV"
    (Just "TAS")
    "CBV"
    (Lat (-42) 33.0)
    (Lon 147 25.0)

_CGB_ ::
  VFR_Waypoint
_CGB_ =
  VFR_Waypoint
    "CRAIGIEBURN OVERPASS"
    (Just "VIC")
    "CGB"
    (Lat (-37) 36.2)
    (Lon 144 56.3)

_CRAY_ ::
  VFR_Waypoint
_CRAY_ =
  VFR_Waypoint
    "CRAYFISH"
    (Just "SA")
    "CRAY"
    (Lat (-38) 35.0)
    (Lon 139 45.0)

_CREM_ ::
  VFR_Waypoint
_CREM_ =
  VFR_Waypoint
    "CREMORNE"
    (Just "TAS")
    "CREM"
    (Lat (-42) 57.5)
    (Lon 147 32.0)

_CES_ ::
  VFR_Waypoint
_CES_ =
  VFR_Waypoint
    "CRESSY"
    (Just "TAS")
    "CES"
    (Lat (-41) 41.5)
    (Lon 147 5.0)

_CWK_ ::
  VFR_Waypoint
_CWK_ =
  VFR_Waypoint
    "CRESWICK"
    (Just "VIC")
    "CWK"
    (Lat (-37) 26.0)
    (Lon 143 54.0)

_CUL_ ::
  VFR_Waypoint
_CUL_ =
  VFR_Waypoint
    "CRONULLA"
    (Just "NSW")
    "CUL"
    (Lat (-34) 3.7)
    (Lon 151 9.2)

_CRPC_ ::
  VFR_Waypoint
_CRPC_ =
  VFR_Waypoint
    "CROPPA CREEK"
    (Just "NSW")
    "CRPC"
    (Lat (-29) 8.0)
    (Lon 150 18.0)

_CNT_ ::
  VFR_Waypoint
_CNT_ =
  VFR_Waypoint
    "CROWS NEST"
    (Just "QLD")
    "CNT"
    (Lat (-27) 16.2)
    (Lon 152 3.3)

_CGD_ ::
  VFR_Waypoint
_CGD_ =
  VFR_Waypoint
    "CUDGEN HEADLAND"
    (Just "NSW")
    "CGD"
    (Lat (-28) 15.9)
    (Lon 153 35.1)

_CCN_ ::
  VFR_Waypoint
_CCN_ =
  VFR_Waypoint
    "CULCAIRN"
    (Just "NSW")
    "CCN"
    (Lat (-35) 40.0)
    (Lon 147 2.3)

_DAIN_ ::
  VFR_Waypoint
_DAIN_ =
  VFR_Waypoint
    "DAINTREE"
    (Just "QLD")
    "DAIN"
    (Lat (-16) 15.0)
    (Lon 145 19.0)

_DLMO_ ::
  VFR_Waypoint
_DLMO_ =
  VFR_Waypoint
    "DALMORE DOWNS"
    (Just "NT")
    "DLMO"
    (Lat (-19) 46.5)
    (Lon 136 0.1)

_DRY_ ::
  VFR_Waypoint
_DRY_ =
  VFR_Waypoint
    "DALRYE"
    (Just "QLD")
    "DRY"
    (Lat (-20) 10.0)
    (Lon 149 4.0)

_DMW_ ::
  VFR_Waypoint
_DMW_ =
  VFR_Waypoint
    "DAM WALL"
    (Just "SA")
    "DMW"
    (Lat (-34) 45.5)
    (Lon 138 43.3)

_DARL_ ::
  VFR_Waypoint
_DARL_ =
  VFR_Waypoint
    "DARLIMURA"
    (Just "VIC")
    "DARL"
    (Lat (-38) 21.2)
    (Lon 146 12.9)

_DHH_ ::
  VFR_Waypoint
_DHH_ =
  VFR_Waypoint
    "DARLING HARBOUR"
    (Just "NSW")
    "DHH"
    (Lat (-33) 51.5)
    (Lon 151 12.0)

_DND_ ::
  VFR_Waypoint
_DND_ =
  VFR_Waypoint
    "DARWIN RIVER DAM"
    (Just "NT")
    "DND"
    (Lat (-12) 49.5)
    (Lon 130 58.0)

_DBO_ ::
  VFR_Waypoint
_DBO_ =
  VFR_Waypoint
    "DAYBORO"
    (Just "QLD")
    "DBO"
    (Lat (-27) 12.0)
    (Lon 152 49.3)

_DFD_ ::
  VFR_Waypoint
_DFD_ =
  VFR_Waypoint
    "DAYLESFORD"
    (Just "VIC")
    "DFD"
    (Lat (-37) 21.0)
    (Lon 144 9.0)

_DGY_ ::
  VFR_Waypoint
_DGY_ =
  VFR_Waypoint
    "DE GREY HS"
    (Just "WA")
    "DGY"
    (Lat (-20) 10.5)
    (Lon 119 10.2)

_DSS_ ::
  VFR_Waypoint
_DSS_ =
  VFR_Waypoint
    "DEDERANG SUBSTATION"
    (Just "VIC")
    "DSS"
    (Lat (-36) 27.2)
    (Lon 146 59.4)

_DPW_ ::
  VFR_Waypoint
_DPW_ =
  VFR_Waypoint
    "DEEP WELL"
    (Just "NT")
    "DPW"
    (Lat (-24) 21.5)
    (Lon 134 3.0)

_DEL_ ::
  VFR_Waypoint
_DEL_ =
  VFR_Waypoint
    "DELORAINE"
    (Just "TAS")
    "DEL"
    (Lat (-41) 32.0)
    (Lon 146 40.0)

_DNP_ ::
  VFR_Waypoint
_DNP_ =
  VFR_Waypoint
    "DENHAM PASSAGE"
    (Just "QLD")
    "DNP"
    (Lat (-11) 20.0)
    (Lon 143 20.0)

_DWB_ ::
  VFR_Waypoint
_DWB_ =
  VFR_Waypoint
    "DERWENT BRIDGE"
    (Just "TAS")
    "DWB"
    (Lat (-42) 8.0)
    (Lon 146 14.0)

_DVM_ ::
  VFR_Waypoint
_DVM_ =
  VFR_Waypoint
    "DEVILS MARBLES"
    (Just "NT")
    "DVM"
    (Lat (-20) 32.0)
    (Lon 134 15.0)

_DIBE_ ::
  VFR_Waypoint
_DIBE_ =
  VFR_Waypoint
    "DICKY BEACH"
    (Just "QLD")
    "DIBE"
    (Lat (-26) 46.9)
    (Lon 153 8.3)

_DODI_ ::
  VFR_Waypoint
_DODI_ =
  VFR_Waypoint
    "DODDS ISLAND"
    (Just "NSW")
    "DODI"
    (Lat (-28) 14.9)
    (Lon 153 32.0)

_DLPT_ ::
  VFR_Waypoint
_DLPT_ =
  VFR_Waypoint
    "DOLLS POINT"
    (Just "NSW")
    "DLPT"
    (Lat (-33) 59.7)
    (Lon 151 8.9)

_DSN_ ::
  VFR_Waypoint
_DSN_ =
  VFR_Waypoint
    "DONCASTER SHOPPINGTOWN"
    (Just "VIC")
    "DSN"
    (Lat (-37) 47.0)
    (Lon 145 7.5)

_DOP_ ::
  VFR_Waypoint
_DOP_ =
  VFR_Waypoint
    "DONNINGTON AIRPARK"
    (Just "QLD")
    "DOP"
    (Lat (-19) 36.1)
    (Lon 146 50.5)

_DOOM_ ::
  VFR_Waypoint
_DOOM_ =
  VFR_Waypoint
    "DOOMBEN RACECOURSE"
    (Just "QLD")
    "DOOM"
    (Lat (-27) 25.6)
    (Lon 153 4.2)

_DRO_ ::
  VFR_Waypoint
_DRO_ =
  VFR_Waypoint
    "DORRIGO"
    (Just "NSW")
    "DRO"
    (Lat (-30) 20.5)
    (Lon 152 42.8)

_DCIS_ ::
  VFR_Waypoint
_DCIS_ =
  VFR_Waypoint
    "DOUBLE CONE ISLAND"
    (Just "QLD")
    "DCIS"
    (Lat (-20) 6.0)
    (Lon 148 43.0)

_DOU_ ::
  VFR_Waypoint
_DOU_ =
  VFR_Waypoint
    "DOUBLE ISLAND"
    (Just "QLD")
    "DOU"
    (Lat (-16) 43.5)
    (Lon 145 41.0)

_DLP_ ::
  VFR_Waypoint
_DLP_ =
  VFR_Waypoint
    "DOUBLE ISLAND PT"
    (Just "QLD")
    "DLP"
    (Lat (-25) 55.0)
    (Lon 153 11.0)

_DBPT_ ::
  VFR_Waypoint
_DBPT_ =
  VFR_Waypoint
    "DOUBLE PT"
    (Just "QLD")
    "DBPT"
    (Lat (-11) 52.0)
    (Lon 142 54.0)

_DRLD_ ::
  VFR_Waypoint
_DRLD_ =
  VFR_Waypoint
    "DREAMWORLD"
    (Just "QLD")
    "DRLD"
    (Lat (-27) 51.9)
    (Lon 153 19.0)

_DRM_ ::
  VFR_Waypoint
_DRM_ =
  VFR_Waypoint
    "DROMANA"
    (Just "VIC")
    "DRM"
    (Lat (-38) 20.0)
    (Lon 144 58.0)

_DRP_ ::
  VFR_Waypoint
_DRP_ =
  VFR_Waypoint
    "DROUGHTY PT"
    (Just "TAS")
    "DRP"
    (Lat (-42) 56.0)
    (Lon 147 25.0)

_DRN_ ::
  VFR_Waypoint
_DRN_ =
  VFR_Waypoint
    "DROUIN"
    (Just "VIC")
    "DRN"
    (Lat (-38) 8.0)
    (Lon 145 51.0)

_DCRK_ ::
  VFR_Waypoint
_DCRK_ =
  VFR_Waypoint
    "DRY CREEK"
    (Just "SA")
    "DCRK"
    (Lat (-34) 50.0)
    (Lon 138 35.0)

_DAG_ ::
  VFR_Waypoint
_DAG_ =
  VFR_Waypoint
    "DUARINGA"
    (Just "QLD")
    "DAG"
    (Lat (-23) 43.2)
    (Lon 149 40.1)

_DUB_ ::
  VFR_Waypoint
_DUB_ =
  VFR_Waypoint
    "DUBLIN"
    (Just "SA")
    "DUB"
    (Lat (-34) 27.3)
    (Lon 138 21.0)

_DLY_ ::
  VFR_Waypoint
_DLY_ =
  VFR_Waypoint
    "DUNALLEY"
    (Just "TAS")
    "DLY"
    (Lat (-42) 53.5)
    (Lon 147 48.3)

_DGN_ ::
  VFR_Waypoint
_DGN_ =
  VFR_Waypoint
    "DUNGOWAN"
    (Just "NSW")
    "DGN"
    (Lat (-31) 13.0)
    (Lon 151 7.0)

_DUWN_ ::
  VFR_Waypoint
_DUWN_ =
  VFR_Waypoint
    "DUNGOWAN DAM"
    (Just "NSW")
    "DUWN"
    (Lat (-31) 24.0)
    (Lon 151 21.0)

_DUA_ ::
  VFR_Waypoint
_DUA_ =
  VFR_Waypoint
    "DURI GAP"
    (Just "NSW")
    "DUA"
    (Lat (-31) 12.0)
    (Lon 150 42.0)

_DMT_ ::
  VFR_Waypoint
_DMT_ =
  VFR_Waypoint
    "DURI MT"
    (Just "NSW")
    "DMT"
    (Lat (-31) 12.3)
    (Lon 150 43.8)

_DONG_ ::
  VFR_Waypoint
_DONG_ =
  VFR_Waypoint
    "DURONG"
    (Just "QLD")
    "DONG"
    (Lat (-26) 24.0)
    (Lon 151 15.0)

_DTON_ ::
  VFR_Waypoint
_DTON_ =
  VFR_Waypoint
    "DUTTON"
    (Just "SA")
    "DTON"
    (Lat (-34) 22.0)
    (Lon 139 8.0)

_EAN_ ::
  VFR_Waypoint
_EAN_ =
  VFR_Waypoint
    "EAGLE HAWK NECK"
    (Just "TAS")
    "EAN"
    (Lat (-43) 1.0)
    (Lon 147 54.0)

_EARH_ ::
  VFR_Waypoint
_EARH_ =
  VFR_Waypoint
    "EARL HILL"
    (Just "QLD")
    "EARH"
    (Lat (-16) 48.0)
    (Lon 145 42.0)

_EARV_ ::
  VFR_Waypoint
_EARV_ =
  VFR_Waypoint
    "EARLVILLE"
    (Just "QLD")
    "EARV"
    (Lat (-16) 57.3)
    (Lon 145 44.3)

_EAM_ ::
  VFR_Waypoint
_EAM_ =
  VFR_Waypoint
    "EAST ARM"
    (Just "NT")
    "EAM"
    (Lat (-12) 31.0)
    (Lon 130 56.0)

_EGT_ ::
  VFR_Waypoint
_EGT_ =
  VFR_Waypoint
    "EAST GRETA"
    (Just "NSW")
    "EGT"
    (Lat (-32) 44.3)
    (Lon 151 32.3)

_ETP_ ::
  VFR_Waypoint
_ETP_ =
  VFR_Waypoint
    "EAST PT"
    (Just "NT")
    "ETP"
    (Lat (-12) 24.5)
    (Lon 130 49.0)

_EDP_ ::
  VFR_Waypoint
_EDP_ =
  VFR_Waypoint
    "EDDYSTONE PT"
    (Just "TAS")
    "EDP"
    (Lat (-40) 59.9)
    (Lon 148 20.9)

_EDT_ ::
  VFR_Waypoint
_EDT_ =
  VFR_Waypoint
    "EDMONTON"
    (Just "QLD")
    "EDT"
    (Lat (-17) 1.5)
    (Lon 145 43.8)

_ELDO_ ::
  VFR_Waypoint
_ELDO_ =
  VFR_Waypoint
    "ELDORADO"
    (Just "VIC")
    "ELDO"
    (Lat (-36) 18.7)
    (Lon 146 31.3)

_ERB_ ::
  VFR_Waypoint
_ERB_ =
  VFR_Waypoint
    "ELIZABETH RIVER BRIDGE"
    (Just "NT")
    "ERB"
    (Lat (-12) 32.6)
    (Lon 130 58.5)

_EMY_ ::
  VFR_Waypoint
_EMY_ =
  VFR_Waypoint
    "EMILY GAP"
    (Just "NT")
    "EMY"
    (Lat (-23) 44.4)
    (Lon 133 56.5)

_EDOR_ ::
  VFR_Waypoint
_EDOR_ =
  VFR_Waypoint
    "ENDEAVOUR REEF"
    (Just "QLD")
    "EDOR"
    (Lat (-15) 47.0)
    (Lon 145 34.0)

_EPPG_ ::
  VFR_Waypoint
_EPPG_ =
  VFR_Waypoint
    "EPPING"
    (Just "VIC")
    "EPPG"
    (Lat (-37) 38.6)
    (Lon 145 1.5)

_ERSK_ ::
  VFR_Waypoint
_ERSK_ =
  VFR_Waypoint
    "ERSKINEVILLE OVAL"
    (Just "NSW")
    "ERSK"
    (Lat (-33) 54.1)
    (Lon 151 11.4)

_ETON_ ::
  VFR_Waypoint
_ETON_ =
  VFR_Waypoint
    "ETON"
    (Just "QLD")
    "ETON"
    (Lat (-21) 16.0)
    (Lon 148 58.3)

_EMI_ ::
  VFR_Waypoint
_EMI_ =
  VFR_Waypoint
    "EUMUNDI"
    (Just "QLD")
    "EMI"
    (Lat (-26) 28.8)
    (Lon 152 57.2)

_EWI_ ::
  VFR_Waypoint
_EWI_ =
  VFR_Waypoint
    "EWANINGA"
    (Just "NT")
    "EWI"
    (Lat (-23) 59.0)
    (Lon 133 56.0)

_EMD_ ::
  VFR_Waypoint
_EMD_ =
  VFR_Waypoint
    "EWEN MADDOCK DAM"
    (Just "QLD")
    "EMD"
    (Lat (-26) 46.7)
    (Lon 153 0.5)

_FCP_ ::
  VFR_Waypoint
_FCP_ =
  VFR_Waypoint
    "FALSE CAPE"
    (Just "QLD")
    "FCP"
    (Lat (-16) 52.5)
    (Lon 145 51.0)

_FHS_ ::
  VFR_Waypoint
_FHS_ =
  VFR_Waypoint
    "FARRER HIGH SCHOOL"
    (Just "NSW")
    "FHS"
    (Lat (-31) 8.5)
    (Lon 150 59.0)

_FDP_ ::
  VFR_Waypoint
_FDP_ =
  VFR_Waypoint
    "FEDERATION PEAK"
    (Just "TAS")
    "FDP"
    (Lat (-43) 16.0)
    (Lon 146 27.0)

_FNVL_ ::
  VFR_Waypoint
_FNVL_ =
  VFR_Waypoint
    "FERNVALE"
    (Just "QLD")
    "FNVL"
    (Lat (-27) 27.3)
    (Lon 152 39.2)

_FLDI_ ::
  VFR_Waypoint
_FLDI_ =
  VFR_Waypoint
    "FIELD ISLAND"
    (Just "NT")
    "FLDI"
    (Lat (-12) 6.0)
    (Lon 132 23.0)

_FISH_ ::
  VFR_Waypoint
_FISH_ =
  VFR_Waypoint
    "FISHERMANS ISLAND"
    (Just "QLD")
    "FISH"
    (Lat (-27) 23.2)
    (Lon 153 10.6)

_FIT_ ::
  VFR_Waypoint
_FIT_ =
  VFR_Waypoint
    "FITNESS CAMP"
    (Just "NSW")
    "FIT"
    (Lat (-35) 10.0)
    (Lon 147 37.3)

_FID_ ::
  VFR_Waypoint
_FID_ =
  VFR_Waypoint
    "FITZROY ISLAND"
    (Just "QLD")
    "FID"
    (Lat (-16) 56.0)
    (Lon 145 59.5)

_FGN_ ::
  VFR_Waypoint
_FGN_ =
  VFR_Waypoint
    "FLEMINGTON"
    (Just "VIC")
    "FGN"
    (Lat (-37) 47.6)
    (Lon 144 54.7)

_FPK_ ::
  VFR_Waypoint
_FPK_ =
  VFR_Waypoint
    "FLINDERS PEAK"
    (Just "QLD")
    "FPK"
    (Lat (-27) 49.0)
    (Lon 152 48.5)

_FYN_ ::
  VFR_Waypoint
_FYN_ =
  VFR_Waypoint
    "FLYNN"
    (Just "VIC")
    "FYN"
    (Lat (-38) 11.6)
    (Lon 146 40.4)

_FOOT_ ::
  VFR_Waypoint
_FOOT_ =
  VFR_Waypoint
    "FOOTBALL PARK"
    (Just "SA")
    "FOOT"
    (Lat (-34) 52.9)
    (Lon 138 29.7)

_FMN_ ::
  VFR_Waypoint
_FMN_ =
  VFR_Waypoint
    "FORMARTIN"
    (Just "QLD")
    "FMN"
    (Lat (-27) 23.8)
    (Lon 151 24.5)

_FDL_ ::
  VFR_Waypoint
_FDL_ =
  VFR_Waypoint
    "FORRESTDALE LAKE"
    (Just "WA")
    "FDL"
    (Lat (-32) 9.6)
    (Lon 115 55.8)

_FFLD_ ::
  VFR_Waypoint
_FFLD_ =
  VFR_Waypoint
    "FORRESTFIELD"
    (Just "WA")
    "FFLD"
    (Lat (-31) 59.3)
    (Lon 116 1.1)

_FOWB_ ::
  VFR_Waypoint
_FOWB_ =
  VFR_Waypoint
    "FOWLERS BAY"
    (Just "SA")
    "FOWB"
    (Lat (-31) 59.0)
    (Lon 132 26.0)

_FRLG_ ::
  VFR_Waypoint
_FRLG_ =
  VFR_Waypoint
    "FREELING"
    (Just "SA")
    "FRLG"
    (Lat (-34) 27.4)
    (Lon 138 48.5)

_FWO_ ::
  VFR_Waypoint
_FWO_ =
  VFR_Waypoint
    "FREEWAY OVERPASS"
    (Just "VIC")
    "FWO"
    (Lat (-37) 47.7)
    (Lon 144 59.6)

_FRE_ ::
  VFR_Waypoint
_FRE_ =
  VFR_Waypoint
    "FREMANTLE"
    (Just "WA")
    "FRE"
    (Lat (-32) 3.5)
    (Lon 115 44.5)

_FREM_ ::
  VFR_Waypoint
_FREM_ =
  VFR_Waypoint
    "FREMANTLE GOLF COURSE"
    (Just "WA")
    "FREM"
    (Lat (-32) 3.3)
    (Lon 115 46.4)

_FRWV_ ::
  VFR_Waypoint
_FRWV_ =
  VFR_Waypoint
    "FRESHWATER VALLEY"
    (Just "QLD")
    "FRWV"
    (Lat (-16) 57.0)
    (Lon 145 41.3)

_GALGA_ ::
  VFR_Waypoint
_GALGA_ =
  VFR_Waypoint
    "GALGA"
    (Just "SA")
    "GALGA"
    (Lat (-34) 43.0)
    (Lon 139 58.0)

_GGN_ ::
  VFR_Waypoint
_GGN_ =
  VFR_Waypoint
    "GALLANGOWAN"
    (Just "QLD")
    "GGN"
    (Lat (-26) 26.0)
    (Lon 152 19.7)

_GAE_ ::
  VFR_Waypoint
_GAE_ =
  VFR_Waypoint
    "GALLILEE"
    (Just "QLD")
    "GAE"
    (Lat (-22) 23.0)
    (Lon 145 59.0)

_GWTR_ ::
  VFR_Waypoint
_GWTR_ =
  VFR_Waypoint
    "GAOL WATER TOWER"
    (Just "NT")
    "GWTR"
    (Lat (-23) 51.5)
    (Lon 133 48.0)

_GST_ ::
  VFR_Waypoint
_GST_ =
  VFR_Waypoint
    "GATE SOUTH"
    (Just "NSW")
    "GST"
    (Lat (-31) 17.4)
    (Lon 150 41.1)

_GWT_ ::
  VFR_Waypoint
_GWT_ =
  VFR_Waypoint
    "GATE WEST"
    (Just "NSW")
    "GWT"
    (Lat (-31) 1.3)
    (Lon 150 34.3)

_GWB_ ::
  VFR_Waypoint
_GWB_ =
  VFR_Waypoint
    "GATEWAY BRIDGE"
    (Just "QLD")
    "GWB"
    (Lat (-27) 26.8)
    (Lon 153 6.0)

_GEK_ ::
  VFR_Waypoint
_GEK_ =
  VFR_Waypoint
    "GEMBROOK"
    (Just "VIC")
    "GEK"
    (Lat (-37) 57.0)
    (Lon 145 33.0)

_GRB_ ::
  VFR_Waypoint
_GRB_ =
  VFR_Waypoint
    "GEORGES RIVER BRIDGE"
    (Just "NSW")
    "GRB"
    (Lat (-34) 0.2)
    (Lon 151 6.6)

_GEP_ ::
  VFR_Waypoint
_GEP_ =
  VFR_Waypoint
    "GEPPS CROSS"
    (Just "SA")
    "GEP"
    (Lat (-34) 51.0)
    (Lon 138 36.0)

_GOY_ ::
  VFR_Waypoint
_GOY_ =
  VFR_Waypoint
    "GEROGERY"
    (Just "NSW")
    "GOY"
    (Lat (-35) 50.3)
    (Lon 146 59.5)

_GNR_ ::
  VFR_Waypoint
_GNR_ =
  VFR_Waypoint
    "GIBSON REEF"
    (Just "QLD")
    "GNR"
    (Lat (-17) 19.0)
    (Lon 146 21.0)

_GIM_ ::
  VFR_Waypoint
_GIM_ =
  VFR_Waypoint
    "GILBERLAND MINE"
    (Just "QLD")
    "GIM"
    (Lat (-19) 18.0)
    (Lon 143 36.0)

_GIRU_ ::
  VFR_Waypoint
_GIRU_ =
  VFR_Waypoint
    "GIRU"
    (Just "QLD")
    "GIRU"
    (Lat (-19) 30.8)
    (Lon 147 6.3)

_GVB_ ::
  VFR_Waypoint
_GVB_ =
  VFR_Waypoint
    "GLADESVILLE BRIDGE"
    (Just "NSW")
    "GVB"
    (Lat (-33) 50.5)
    (Lon 151 8.8)

_GBRY_ ::
  VFR_Waypoint
_GBRY_ =
  VFR_Waypoint
    "GLEN BRAY"
    (Just "NSW")
    "GBRY"
    (Lat (-35) 55.5)
    (Lon 147 0.5)

_GBR_ ::
  VFR_Waypoint
_GBR_ =
  VFR_Waypoint
    "GLENBURN"
    (Just "VIC")
    "GBR"
    (Lat (-37) 25.5)
    (Lon 145 25.3)

_GLEN_ ::
  VFR_Waypoint
_GLEN_ =
  VFR_Waypoint
    "GLENLOCH INTERCHANGE"
    (Just "NSW")
    "GLEN"
    (Lat (-35) 17.1)
    (Lon 149 5.1)

_GMN_ ::
  VFR_Waypoint
_GMN_ =
  VFR_Waypoint
    "GLENMORGAN"
    (Just "QLD")
    "GMN"
    (Lat (-27) 15.0)
    (Lon 149 41.0)

_GLC_ ::
  VFR_Waypoint
_GLC_ =
  VFR_Waypoint
    "GLENORCHY"
    (Just "SA")
    "GLC"
    (Lat (-31) 55.0)
    (Lon 139 47.0)

_GRE_ ::
  VFR_Waypoint
_GRE_ =
  VFR_Waypoint
    "GLENREAGH"
    (Just "NSW")
    "GRE"
    (Lat (-30) 3.3)
    (Lon 152 58.7)

_GRK_ ::
  VFR_Waypoint
_GRK_ =
  VFR_Waypoint
    "GLENROCK HS"
    (Just "QLD")
    "GRK"
    (Lat (-15) 7.0)
    (Lon 145 5.0)

_GCR_ ::
  VFR_Waypoint
_GCR_ =
  VFR_Waypoint
    "GLOUCESTER"
    (Just "NSW")
    "GCR"
    (Lat (-32) 0.3)
    (Lon 151 58.0)

_GMH_ ::
  VFR_Waypoint
_GMH_ =
  VFR_Waypoint
    "GMH"
    (Just "VIC")
    "GMH"
    (Lat (-38) 0.5)
    (Lon 145 14.3)

_GOI_ ::
  VFR_Waypoint
_GOI_ =
  VFR_Waypoint
    "GOLDSMITH ISLAND"
    (Just "QLD")
    "GOI"
    (Lat (-20) 41.0)
    (Lon 149 9.0)

_GON_ ::
  VFR_Waypoint
_GON_ =
  VFR_Waypoint
    "GOODNA"
    (Just "QLD")
    "GON"
    (Lat (-27) 37.0)
    (Lon 152 53.3)

_GGV_ ::
  VFR_Waypoint
_GGV_ =
  VFR_Waypoint
    "GOOGONG RESV"
    (Just "NSW")
    "GGV"
    (Lat (-35) 25.3)
    (Lon 149 15.7)

_GMBG_ ::
  VFR_Waypoint
_GMBG_ =
  VFR_Waypoint
    "GOOMBUNGEE"
    (Just "QLD")
    "GMBG"
    (Lat (-27) 18.5)
    (Lon 151 51.0)

_GNN_ ::
  VFR_Waypoint
_GNN_ =
  VFR_Waypoint
    "GOONANEMAN"
    (Just "QLD")
    "GNN"
    (Lat (-25) 26.0)
    (Lon 152 8.0)

_GGO_ ::
  VFR_Waypoint
_GGO_ =
  VFR_Waypoint
    "GOONOO GOONOO HS"
    (Just "NSW")
    "GGO"
    (Lat (-31) 18.6)
    (Lon 150 54.3)

_GOV_ ::
  VFR_Waypoint
_GOV_ =
  VFR_Waypoint
    "GORDONVALE"
    (Just "QLD")
    "GOV"
    (Lat (-17) 5.3)
    (Lon 145 47.0)

_GOW_ ::
  VFR_Waypoint
_GOW_ =
  VFR_Waypoint
    "GOWRIE JUNCTION"
    (Just "QLD")
    "GOW"
    (Lat (-27) 30.0)
    (Lon 151 53.2)

_GRAA_ ::
  VFR_Waypoint
_GRAA_ =
  VFR_Waypoint
    "GRANYA"
    (Just "VIC")
    "GRAA"
    (Lat (-36) 6.7)
    (Lon 147 19.0)

_GVH_ ::
  VFR_Waypoint
_GVH_ =
  VFR_Waypoint
    "GRAVELLY BEACH"
    (Just "TAS")
    "GVH"
    (Lat (-41) 17.3)
    (Lon 146 58.3)

_GRHL_ ::
  VFR_Waypoint
_GRHL_ =
  VFR_Waypoint
    "GREEN HILLS"
    (Just "WA")
    "GRHL"
    (Lat (-31) 55.5)
    (Lon 116 57.5)

_GNIS_ ::
  VFR_Waypoint
_GNIS_ =
  VFR_Waypoint
    "GREEN ISLAND (CAIRNS)"
    (Just "QLD")
    "GNIS"
    (Lat (-16) 45.5)
    (Lon 145 58.5)

_GRL_ ::
  VFR_Waypoint
_GRL_ =
  VFR_Waypoint
    "GREEN ISLAND (MACKAY)"
    (Just "QLD")
    "GRL"
    (Lat (-20) 59.0)
    (Lon 149 9.0)

_GRNH_ ::
  VFR_Waypoint
_GRNH_ =
  VFR_Waypoint
    "GREENHILL"
    (Just "QLD")
    "GRNH"
    (Lat (-17) 2.4)
    (Lon 145 48.3)

_GNM_ ::
  VFR_Waypoint
_GNM_ =
  VFR_Waypoint
    "GREENMOUNT"
    (Just "QLD")
    "GNM"
    (Lat (-27) 47.0)
    (Lon 151 57.0)

_GRRV_ ::
  VFR_Waypoint
_GRRV_ =
  VFR_Waypoint
    "GROSE RIVER"
    (Just "NSW")
    "GRRV"
    (Lat (-33) 36.9)
    (Lon 150 40.2)

_GDG_ ::
  VFR_Waypoint
_GDG_ =
  VFR_Waypoint
    "GUNDAGAI"
    (Just "NSW")
    "GDG"
    (Lat (-35) 3.7)
    (Lon 148 6.2)

_GUP_ ::
  VFR_Waypoint
_GUP_ =
  VFR_Waypoint
    "GUNN PT"
    (Just "NT")
    "GUP"
    (Lat (-12) 11.0)
    (Lon 130 59.5)

_GUNN_ ::
  VFR_Waypoint
_GUNN_ =
  VFR_Waypoint
    "GUNNING"
    (Just "NSW")
    "GUNN"
    (Lat (-34) 46.9)
    (Lon 149 16.0)

_GUNG_ ::
  VFR_Waypoint
_GUNG_ =
  VFR_Waypoint
    "GUTHALUNGRA"
    (Just "QLD")
    "GUNG"
    (Lat (-19) 56.0)
    (Lon 147 50.0)

_HADEN_ ::
  VFR_Waypoint
_HADEN_ =
  VFR_Waypoint
    "HADEN"
    (Just "QLD")
    "HADEN"
    (Lat (-27) 13.2)
    (Lon 151 53.3)

_HSP_ ::
  VFR_Waypoint
_HSP_ =
  VFR_Waypoint
    "HADSPEN"
    (Just "TAS")
    "HSP"
    (Lat (-41) 30.5)
    (Lon 147 3.5)

_HALL_ ::
  VFR_Waypoint
_HALL_ =
  VFR_Waypoint
    "HALL"
    (Just "ACT")
    "HALL"
    (Lat (-35) 10.1)
    (Lon 149 4.1)

_HMM_ ::
  VFR_Waypoint
_HMM_ =
  VFR_Waypoint
    "HAMMOND ISLAND"
    (Just "QLD")
    "HMM"
    (Lat (-10) 32.0)
    (Lon 142 13.0)

_HMN_ ::
  VFR_Waypoint
_HMN_ =
  VFR_Waypoint
    "HAMPTON"
    (Just "QLD")
    "HMN"
    (Lat (-27) 21.5)
    (Lon 152 4.0)

_HAVY_ ::
  VFR_Waypoint
_HAVY_ =
  VFR_Waypoint
    "HAPPY VALLEY"
    (Just "QLD")
    "HAVY"
    (Lat (-26) 48.5)
    (Lon 153 8.0)

_HBB_ ::
  VFR_Waypoint
_HBB_ =
  VFR_Waypoint
    "HARBOUR BRIDGE"
    (Just "NSW")
    "HBB"
    (Lat (-33) 51.2)
    (Lon 151 12.5)

_HAF_ ::
  VFR_Waypoint
_HAF_ =
  VFR_Waypoint
    "HAREFIELD"
    (Just "NSW")
    "HAF"
    (Lat (-34) 57.8)
    (Lon 147 31.0)

_HGTE_ ::
  VFR_Waypoint
_HGTE_ =
  VFR_Waypoint
    "HARROGATE"
    (Just "SA")
    "HGTE"
    (Lat (-34) 57.2)
    (Lon 139 1.1)

_HARV_ ::
  VFR_Waypoint
_HARV_ =
  VFR_Waypoint
    "HARVEY"
    (Just "WA")
    "HARV"
    (Lat (-33) 5.0)
    (Lon 115 54.0)

_HSTI_ ::
  VFR_Waypoint
_HSTI_ =
  VFR_Waypoint
    "HASTINGS"
    (Just "QLD")
    "HSTI"
    (Lat (-28) 18.0)
    (Lon 145 11.0)

_HASS_ ::
  VFR_Waypoint
_HASS_ =
  VFR_Waypoint
    "HASTINGS PT"
    (Just "NSW")
    "HASS"
    (Lat (-28) 21.5)
    (Lon 153 34.8)

_HATF_ ::
  VFR_Waypoint
_HATF_ =
  VFR_Waypoint
    "HATFIELD"
    (Just "NSW")
    "HATF"
    (Lat (-33) 42.0)
    (Lon 143 39.0)

_HVI_ ::
  VFR_Waypoint
_HVI_ =
  VFR_Waypoint
    "HAVANNAH ISLAND"
    (Just "QLD")
    "HVI"
    (Lat (-18) 50.7)
    (Lon 146 32.3)

_HPT_ ::
  VFR_Waypoint
_HPT_ =
  VFR_Waypoint
    "HAY PT"
    (Just "QLD")
    "HPT"
    (Lat (-21) 16.7)
    (Lon 149 17.5)

_HAZ_ ::
  VFR_Waypoint
_HAZ_ =
  VFR_Waypoint
    "HAZELWOOD ISLAND"
    (Just "QLD")
    "HAZ"
    (Lat (-20) 17.0)
    (Lon 149 5.0)

_HEAT_ ::
  VFR_Waypoint
_HEAT_ =
  VFR_Waypoint
    "HEATHCOTE TOWNSHIP"
    (Just "VIC")
    "HEAT"
    (Lat (-36) 55.0)
    (Lon 144 42.0)

_HRR_ ::
  VFR_Waypoint
_HRR_ =
  VFR_Waypoint
    "HELENA RIVER RESV"
    (Just "WA")
    "HRR"
    (Lat (-32) 0.1)
    (Lon 116 13.6)

_HNB_ ::
  VFR_Waypoint
_HNB_ =
  VFR_Waypoint
    "HELENSBURGH"
    (Just "NSW")
    "HNB"
    (Lat (-34) 11.5)
    (Lon 150 58.5)

_HED_ ::
  VFR_Waypoint
_HED_ =
  VFR_Waypoint
    "HELIDON"
    (Just "QLD")
    "HED"
    (Lat (-27) 33.0)
    (Lon 152 8.0)

_HENTY_ ::
  VFR_Waypoint
_HENTY_ =
  VFR_Waypoint
    "HENTY"
    (Just "NSW")
    "HENTY"
    (Lat (-35) 31.5)
    (Lon 147 2.0)

_HKE_ ::
  VFR_Waypoint
_HKE_ =
  VFR_Waypoint
    "HERDSMAN LAKE"
    (Just "WA")
    "HKE"
    (Lat (-31) 55.2)
    (Lon 115 48.8)

_HXB_ ::
  VFR_Waypoint
_HXB_ =
  VFR_Waypoint
    "HEXHAM BRIDGE"
    (Just "NSW")
    "HXB"
    (Lat (-32) 49.8)
    (Lon 151 41.4)

_HIPA_ ::
  VFR_Waypoint
_HIPA_ =
  VFR_Waypoint
    "HIGH PERFORMANCE AREA 3"
    (Just "QLD")
    "HIPA"
    (Lat (-27) 9.0)
    (Lon 153 50.8)

_HVTG_ ::
  VFR_Waypoint
_HVTG_ =
  VFR_Waypoint
    "HIGH VOLTAGE"
    (Just "QLD")
    "HVTG"
    (Lat (-17) 0.9)
    (Lon 145 45.8)

_HIM_ ::
  VFR_Waypoint
_HIM_ =
  VFR_Waypoint
    "HILTON MINE"
    (Just "QLD")
    "HIM"
    (Lat (-20) 34.0)
    (Lon 139 29.0)

_HNCH_ ::
  VFR_Waypoint
_HNCH_ =
  VFR_Waypoint
    "HINCHINBROOK ISLAND"
    (Just "QLD")
    "HNCH"
    (Lat (-18) 22.0)
    (Lon 146 15.0)

_HDWL_ ::
  VFR_Waypoint
_HDWL_ =
  VFR_Waypoint
    "HINZE DAM WALL"
    (Just "QLD")
    "HDWL"
    (Lat (-28) 3.0)
    (Lon 153 17.2)

_HCTY_ ::
  VFR_Waypoint
_HCTY_ =
  VFR_Waypoint
    "HOBART CBD"
    (Just "TAS")
    "HCTY"
    (Lat (-42) 53.0)
    (Lon 147 20.0)

_HBKT_ ::
  VFR_Waypoint
_HBKT_ =
  VFR_Waypoint
    "HOLBROOK TOWNSHIP"
    (Just "NSW")
    "HBKT"
    (Lat (-35) 43.8)
    (Lon 147 19.5)

_HOLM_ ::
  VFR_Waypoint
_HOLM_ =
  VFR_Waypoint
    "HOLMES REEF"
    (Just "QLD")
    "HOLM"
    (Lat (-16) 30.0)
    (Lon 148 0.0)

_HBU_ ::
  VFR_Waypoint
_HBU_ =
  VFR_Waypoint
    "HOMEBUSH"
    (Just "QLD")
    "HBU"
    (Lat (-21) 16.8)
    (Lon 149 3.0)

_HDP_ ::
  VFR_Waypoint
_HDP_ =
  VFR_Waypoint
    "HOOD PT"
    (Just "WA")
    "HDP"
    (Lat (-34) 23.0)
    (Lon 119 34.0)

_HPI_ ::
  VFR_Waypoint
_HPI_ =
  VFR_Waypoint
    "HOPE INLET"
    (Just "NT")
    "HPI"
    (Lat (-12) 19.7)
    (Lon 131 1.0)

_HVR_ ::
  VFR_Waypoint
_HVR_ =
  VFR_Waypoint
    "HOPE VALLEY RESV"
    (Just "SA")
    "HVR"
    (Lat (-34) 51.0)
    (Lon 138 41.0)

_HORD_ ::
  VFR_Waypoint
_HORD_ =
  VFR_Waypoint
    "HORDERN HILLS"
    (Just "NT")
    "HORD"
    (Lat (-20) 39.0)
    (Lon 130 19.0)

_HZWF_ ::
  VFR_Waypoint
_HZWF_ =
  VFR_Waypoint
    "HORIZONTAL WATERFALLS"
    (Just "WA")
    "HZWF"
    (Lat (-16) 23.0)
    (Lon 123 58.0)

_HBVT_ ::
  VFR_Waypoint
_HBVT_ =
  VFR_Waypoint
    "HORNIBROOK VIADUCT"
    (Just "QLD")
    "HBVT"
    (Lat (-27) 16.4)
    (Lon 153 4.3)

_HSY_ ::
  VFR_Waypoint
_HSY_ =
  VFR_Waypoint
    "HORNSBY"
    (Just "NSW")
    "HSY"
    (Lat (-33) 41.5)
    (Lon 151 6.4)

_HST_ ::
  VFR_Waypoint
_HST_ =
  VFR_Waypoint
    "HOSKINSTOWN"
    (Just "NSW")
    "HST"
    (Lat (-35) 25.3)
    (Lon 149 27.0)

_HWG_ ::
  VFR_Waypoint
_HWG_ =
  VFR_Waypoint
    "HOWLONG"
    (Just "NSW")
    "HWG"
    (Lat (-35) 58.6)
    (Lon 146 37.5)

_HGR_ ::
  VFR_Waypoint
_HGR_ =
  VFR_Waypoint
    "HUGH RIVER"
    (Just "NT")
    "HGR"
    (Lat (-24) 21.0)
    (Lon 133 26.0)

_HWW_ ::
  VFR_Waypoint
_HWW_ =
  VFR_Waypoint
    "HUME WEIR WALL"
    (Just "VIC")
    "HWW"
    (Lat (-36) 6.7)
    (Lon 147 1.5)

_HYH_ ::
  VFR_Waypoint
_HYH_ =
  VFR_Waypoint
    "HUMMOCKY HILLS"
    (Just "TAS")
    "HYH"
    (Lat (-41) 44.0)
    (Lon 147 15.0)

_HRD_ ::
  VFR_Waypoint
_HRD_ =
  VFR_Waypoint
    "HUNGERFORD"
    (Just "NSW")
    "HRD"
    (Lat (-29) 0.0)
    (Lon 144 24.0)

_HYDEN_ ::
  VFR_Waypoint
_HYDEN_ =
  VFR_Waypoint
    "HYDEN"
    (Just "WA")
    "HYDEN"
    (Lat (-32) 27.0)
    (Lon 118 52.0)

_IND_ ::
  VFR_Waypoint
_IND_ =
  VFR_Waypoint
    "INDEE HS"
    (Just "WA")
    "IND"
    (Lat (-20) 47.2)
    (Lon 118 35.5)

_IDNA_ ::
  VFR_Waypoint
_IDNA_ =
  VFR_Waypoint
    "INDIANA"
    (Just "NT")
    "IDNA"
    (Lat (-23) 20.0)
    (Lon 135 26.0)

_IDK_ ::
  VFR_Waypoint
_IDK_ =
  VFR_Waypoint
    "INDULKANA TOWNSHIP"
    (Just "SA")
    "IDK"
    (Lat (-26) 58.0)
    (Lon 133 19.5)

_INGL_ ::
  VFR_Waypoint
_INGL_ =
  VFR_Waypoint
    "INGLEBURN"
    (Just "NSW")
    "INGL"
    (Lat (-33) 58.3)
    (Lon 150 51.5)

_IPHL_ ::
  VFR_Waypoint
_IPHL_ =
  VFR_Waypoint
    "IPPIA HILL"
    (Just "NT")
    "IPHL"
    (Lat (-25) 7.0)
    (Lon 133 3.0)

_ISB_ ::
  VFR_Waypoint
_ISB_ =
  VFR_Waypoint
    "ISRAELITE BAY"
    (Just "WA")
    "ISB"
    (Lat (-33) 37.0)
    (Lon 123 53.0)

_JSK_ ::
  VFR_Waypoint
_JSK_ =
  VFR_Waypoint
    "JACK SMITH LAKE"
    (Just "VIC")
    "JSK"
    (Lat (-38) 30.0)
    (Lon 147 0.0)

_JAC_ ::
  VFR_Waypoint
_JAC_ =
  VFR_Waypoint
    "JACKO'S JUNCTION"
    (Just "NT")
    "JAC"
    (Lat (-12) 15.8)
    (Lon 131 2.2)

_JNR_ ::
  VFR_Waypoint
_JNR_ =
  VFR_Waypoint
    "JACKSON RIVER"
    (Just "QLD")
    "JNR"
    (Lat (-11) 35.0)
    (Lon 142 0.0)

_JSL_ ::
  VFR_Waypoint
_JSL_ =
  VFR_Waypoint
    "JACOB'S SUGARLOAF"
    (Just "TAS")
    "JSL"
    (Lat (-41) 56.5)
    (Lon 147 18.0)

_JACW_ ::
  VFR_Waypoint
_JACW_ =
  VFR_Waypoint
    "JACOB'S WELL"
    (Just "WA")
    "JACW"
    (Lat (-32) 2.0)
    (Lon 117 12.0)

_JMPP_ ::
  VFR_Waypoint
_JMPP_ =
  VFR_Waypoint
    "JAMES PRICE POINT"
    (Just "WA")
    "JMPP"
    (Lat (-17) 29.1)
    (Lon 122 8.6)

_JADL_ ::
  VFR_Waypoint
_JADL_ =
  VFR_Waypoint
    "JARRAHDALE"
    (Just "WA")
    "JADL"
    (Lat (-32) 20.4)
    (Lon 116 4.5)

_JCK_ ::
  VFR_Waypoint
_JCK_ =
  VFR_Waypoint
    "JAY CREEK"
    (Just "NT")
    "JCK"
    (Lat (-23) 47.2)
    (Lon 133 30.0)

_JEA_ ::
  VFR_Waypoint
_JEA_ =
  VFR_Waypoint
    "JEANNIE RIVER"
    (Just "QLD")
    "JEA"
    (Lat (-14) 44.0)
    (Lon 144 52.0)

_JES_ ::
  VFR_Waypoint
_JES_ =
  VFR_Waypoint
    "JESSIE GAP"
    (Just "NT")
    "JES"
    (Lat (-23) 44.9)
    (Lon 134 1.1)

_JIBN_ ::
  VFR_Waypoint
_JIBN_ =
  VFR_Waypoint
    "JIBBON PT"
    (Just "NSW")
    "JIBN"
    (Lat (-34) 5.1)
    (Lon 151 10.2)

_JIA_ ::
  VFR_Waypoint
_JIA_ =
  VFR_Waypoint
    "JINDERA"
    (Just "NSW")
    "JIA"
    (Lat (-35) 57.5)
    (Lon 146 53.3)

_JDN_ ::
  VFR_Waypoint
_JDN_ =
  VFR_Waypoint
    "JONDARYAN"
    (Just "QLD")
    "JDN"
    (Lat (-27) 22.0)
    (Lon 151 35.3)

_JGK_ ::
  VFR_Waypoint
_JGK_ =
  VFR_Waypoint
    "JUG CREEK"
    (Just "QLD")
    "JGK"
    (Lat (-22) 0.0)
    (Lon 144 42.0)

_JPP_ ::
  VFR_Waypoint
_JPP_ =
  VFR_Waypoint
    "JUMPINPIN"
    (Just "QLD")
    "JPP"
    (Lat (-27) 44.0)
    (Lon 153 27.0)

_JUNEE_ ::
  VFR_Waypoint
_JUNEE_ =
  VFR_Waypoint
    "JUNEE"
    (Just "NSW")
    "JUNEE"
    (Lat (-34) 52.0)
    (Lon 147 35.0)

_JUP_ ::
  VFR_Waypoint
_JUP_ =
  VFR_Waypoint
    "JUPITERS CASINO"
    (Just "QLD")
    "JUP"
    (Lat (-28) 1.9)
    (Lon 153 25.8)

_KKN_ ::
  VFR_Waypoint
_KKN_ =
  VFR_Waypoint
    "KAIMKILLENBUN"
    (Just "QLD")
    "KKN"
    (Lat (-27) 4.0)
    (Lon 151 26.0)

_KAO_ ::
  VFR_Waypoint
_KAO_ =
  VFR_Waypoint
    "KALKALLO"
    (Just "VIC")
    "KAO"
    (Lat (-37) 32.2)
    (Lon 144 56.7)

_KTS_ ::
  VFR_Waypoint
_KTS_ =
  VFR_Waypoint
    "KEATS ISLAND"
    (Just "QLD")
    "KTS"
    (Lat (-9) 41.0)
    (Lon 143 27.0)

_KEP_ ::
  VFR_Waypoint
_KEP_ =
  VFR_Waypoint
    "KEEPIT DAM"
    (Just "NSW")
    "KEP"
    (Lat (-30) 52.8)
    (Lon 150 29.9)

_KERW_ ::
  VFR_Waypoint
_KERW_ =
  VFR_Waypoint
    "KEERWEE"
    (Just "QLD")
    "KERW"
    (Lat (-25) 13.0)
    (Lon 151 21.0)

_KALL_ ::
  VFR_Waypoint
_KALL_ =
  VFR_Waypoint
    "KENDALL"
    (Just "QLD")
    "KALL"
    (Lat (-14) 12.0)
    (Lon 141 36.0)

_KSI_ ::
  VFR_Waypoint
_KSI_ =
  VFR_Waypoint
    "KESWICK ISLAND"
    (Just "QLD")
    "KSI"
    (Lat (-20) 55.0)
    (Lon 149 25.0)

_KMA_ ::
  VFR_Waypoint
_KMA_ =
  VFR_Waypoint
    "KIAMA"
    (Just "NSW")
    "KMA"
    (Lat (-34) 40.0)
    (Lon 150 51.0)

_KIAN_ ::
  VFR_Waypoint
_KIAN_ =
  VFR_Waypoint
    "KIANDRA"
    (Just "NSW")
    "KIAN"
    (Lat (-35) 52.0)
    (Lon 148 30.0)

_KDBF_ ::
  VFR_Waypoint
_KDBF_ =
  VFR_Waypoint
    "KIDSON BLUFF"
    (Just "WA")
    "KDBF"
    (Lat (-22) 15.0)
    (Lon 125 2.0)

_KIEWA_ ::
  VFR_Waypoint
_KIEWA_ =
  VFR_Waypoint
    "KIEWA"
    (Just "VIC")
    "KIEWA"
    (Lat (-36) 15.5)
    (Lon 147 0.5)

_KLCY_ ::
  VFR_Waypoint
_KLCY_ =
  VFR_Waypoint
    "KILCOY TOWNSHIP"
    (Just "QLD")
    "KLCY"
    (Lat (-26) 56.5)
    (Lon 152 33.8)

_KKV_ ::
  VFR_Waypoint
_KKV_ =
  VFR_Waypoint
    "KILKIVAN"
    (Just "QLD")
    "KKV"
    (Lat (-26) 5.0)
    (Lon 152 14.8)

_KIM_ ::
  VFR_Waypoint
_KIM_ =
  VFR_Waypoint
    "KILMORE"
    (Just "VIC")
    "KIM"
    (Lat (-37) 18.0)
    (Lon 144 57.3)

_KMG_ ::
  VFR_Waypoint
_KMG_ =
  VFR_Waypoint
    "KILMORE GAP"
    (Just "VIC")
    "KMG"
    (Lat (-37) 18.0)
    (Lon 144 59.0)

_KLTO_ ::
  VFR_Waypoint
_KLTO_ =
  VFR_Waypoint
    "KILTO"
    (Just "WA")
    "KLTO"
    (Lat (-17) 41.5)
    (Lon 122 42.8)

_KCAS_ ::
  VFR_Waypoint
_KCAS_ =
  VFR_Waypoint
    "KING CASCADES"
    (Just "WA")
    "KCAS"
    (Lat (-15) 37.5)
    (Lon 125 18.0)

_KRT_ ::
  VFR_Waypoint
_KRT_ =
  VFR_Waypoint
    "KING RANCH TULLY"
    (Just "QLD")
    "KRT"
    (Lat (-18) 5.0)
    (Lon 145 50.0)

_KGLE_ ::
  VFR_Waypoint
_KGLE_ =
  VFR_Waypoint
    "KINGLAKE"
    (Just "VIC")
    "KGLE"
    (Lat (-37) 31.3)
    (Lon 145 21.0)

_KBCH_ ::
  VFR_Waypoint
_KBCH_ =
  VFR_Waypoint
    "KINGS BEACH"
    (Just "QLD")
    "KBCH"
    (Lat (-26) 48.3)
    (Lon 153 8.4)

_KCFF_ ::
  VFR_Waypoint
_KCFF_ =
  VFR_Waypoint
    "KINGSCLIFF"
    (Just "NSW")
    "KCFF"
    (Lat (-28) 15.0)
    (Lon 153 34.3)

_KGT_ ::
  VFR_Waypoint
_KGT_ =
  VFR_Waypoint
    "KINGSTHORPE"
    (Just "QLD")
    "KGT"
    (Lat (-27) 28.7)
    (Lon 151 48.8)

_KINN_ ::
  VFR_Waypoint
_KINN_ =
  VFR_Waypoint
    "KINGSTON CENTRE"
    (Just "VIC")
    "KINN"
    (Lat (-37) 57.4)
    (Lon 145 4.6)

_KIRA_ ::
  VFR_Waypoint
_KIRA_ =
  VFR_Waypoint
    "KIRRA"
    (Just "NSW")
    "KIRA"
    (Lat (-28) 10.0)
    (Lon 153 31.4)

_KSPT_ ::
  VFR_Waypoint
_KSPT_ =
  VFR_Waypoint
    "KISSING POINT"
    (Just "QLD")
    "KSPT"
    (Lat (-19) 14.5)
    (Lon 146 48.3)

_KNW_ ::
  VFR_Waypoint
_KNW_ =
  VFR_Waypoint
    "KONGWAK"
    (Just "VIC")
    "KNW"
    (Lat (-38) 31.0)
    (Lon 145 43.0)

_KTG_ ::
  VFR_Waypoint
_KTG_ =
  VFR_Waypoint
    "KOOLATONG RIVER"
    (Just "NT")
    "KTG"
    (Lat (-13) 5.0)
    (Lon 135 39.0)

_KBD_ ::
  VFR_Waypoint
_KBD_ =
  VFR_Waypoint
    "KOOMBOOLOOMBA DAM"
    (Just "QLD")
    "KBD"
    (Lat (-17) 50.0)
    (Lon 145 36.0)

_KOOM_ ::
  VFR_Waypoint
_KOOM_ =
  VFR_Waypoint
    "KOOMOOLOOBOOKA CAVE"
    (Just "SA")
    "KOOM"
    (Lat (-31) 29.0)
    (Lon 129 35.0)

_KANC_ ::
  VFR_Waypoint
_KANC_ =
  VFR_Waypoint
    "KOORAN CROCODILE FARM"
    (Just "QLD")
    "KANC"
    (Lat (-23) 18.0)
    (Lon 150 44.0)

_KOT_ ::
  VFR_Waypoint
_KOT_ =
  VFR_Waypoint
    "KOOTINGAL"
    (Just "NSW")
    "KOT"
    (Lat (-31) 3.0)
    (Lon 151 3.0)

_KOPP_ ::
  VFR_Waypoint
_KOPP_ =
  VFR_Waypoint
    "KOPPEN PARK"
    (Just "QLD")
    "KOPP"
    (Lat (-16) 55.9)
    (Lon 145 44.0)

_KREE_ ::
  VFR_Waypoint
_KREE_ =
  VFR_Waypoint
    "KOREELAH"
    (Just "SA")
    "KREE"
    (Lat (-35) 55.0)
    (Lon 136 55.0)

_KUN_ ::
  VFR_Waypoint
_KUN_ =
  VFR_Waypoint
    "KUNWARARA"
    (Just "QLD")
    "KUN"
    (Lat (-22) 55.0)
    (Lon 150 8.0)

_KRN_ ::
  VFR_Waypoint
_KRN_ =
  VFR_Waypoint
    "KURANDA"
    (Just "QLD")
    "KRN"
    (Lat (-16) 49.0)
    (Lon 145 38.3)

_KRMD_ ::
  VFR_Waypoint
_KRMD_ =
  VFR_Waypoint
    "KURMOND"
    (Just "NSW")
    "KRMD"
    (Lat (-33) 33.0)
    (Lon 150 41.2)

_KYP_ ::
  VFR_Waypoint
_KYP_ =
  VFR_Waypoint
    "KYEAMBA PARK"
    (Just "NSW")
    "KYP"
    (Lat (-35) 26.8)
    (Lon 147 37.0)

_KYE_ ::
  VFR_Waypoint
_KYE_ =
  VFR_Waypoint
    "KYEAMBA TOWER"
    (Just "NSW")
    "KYE"
    (Lat (-35) 31.5)
    (Lon 147 35.5)

_KTN_ ::
  VFR_Waypoint
_KTN_ =
  VFR_Waypoint
    "KYNETON"
    (Just "VIC")
    "KTN"
    (Lat (-37) 14.8)
    (Lon 144 27.5)

_LDH_ ::
  VFR_Waypoint
_LDH_ =
  VFR_Waypoint
    "LADYSMITH"
    (Just "NSW")
    "LDH"
    (Lat (-35) 12.7)
    (Lon 147 31.0)

_LDLY_ ::
  VFR_Waypoint
_LDLY_ =
  VFR_Waypoint
    "LAIDLEY"
    (Just "QLD")
    "LDLY"
    (Lat (-27) 38.0)
    (Lon 152 23.0)

_LKT_ ::
  VFR_Waypoint
_LKT_ =
  VFR_Waypoint
    "LAKE ALBERT"
    (Just "NSW")
    "LKT"
    (Lat (-35) 10.7)
    (Lon 147 21.8)

_LKAD_ ::
  VFR_Waypoint
_LKAD_ =
  VFR_Waypoint
    "LAKE AMADEUS"
    (Just "NT")
    "LKAD"
    (Lat (-24) 34.8)
    (Lon 130 27.7)

_LBT_ ::
  VFR_Waypoint
_LBT_ =
  VFR_Waypoint
    "LAKE BATHURST"
    (Just "NSW")
    "LBT"
    (Lat (-35) 3.0)
    (Lon 149 41.0)

_LKON_ ::
  VFR_Waypoint
_LKON_ =
  VFR_Waypoint
    "LAKE CLARENDON"
    (Just "QLD")
    "LKON"
    (Lat (-27) 29.5)
    (Lon 152 21.0)

_LCR_ ::
  VFR_Waypoint
_LCR_ =
  VFR_Waypoint
    "LAKE CORANGAMITE"
    (Just "VIC")
    "LCR"
    (Lat (-38) 6.0)
    (Lon 143 31.0)

_LCDI_ ::
  VFR_Waypoint
_LCDI_ =
  VFR_Waypoint
    "LAKE CURRIMUNDI"
    (Just "QLD")
    "LCDI"
    (Lat (-26) 45.9)
    (Lon 153 8.2)

_LAD_ ::
  VFR_Waypoint
_LAD_ =
  VFR_Waypoint
    "LAKE DEAN"
    (Just "NT")
    "LAD"
    (Lat (-12) 44.0)
    (Lon 131 1.0)

_LDAP_ ::
  VFR_Waypoint
_LDAP_ =
  VFR_Waypoint
    "LAKE DISAPPOINTMENT"
    (Just "WA")
    "LDAP"
    (Lat (-23) 30.0)
    (Lon 122 40.0)

_LKEC_ ::
  VFR_Waypoint
_LKEC_ =
  VFR_Waypoint
    "LAKE ECHO"
    (Just "TAS")
    "LKEC"
    (Lat (-42) 10.0)
    (Lon 146 38.0)

_LEM_ ::
  VFR_Waypoint
_LEM_ =
  VFR_Waypoint
    "LAKE EPSOM"
    (Just "QLD")
    "LEM"
    (Lat (-21) 29.3)
    (Lon 148 50.0)

_LEYN_ ::
  VFR_Waypoint
_LEYN_ =
  VFR_Waypoint
    "LAKE EYRE NORTH"
    (Just "SA")
    "LEYN"
    (Lat (-28) 25.0)
    (Lon 137 18.0)

_LFS_ ::
  VFR_Waypoint
_LFS_ =
  VFR_Waypoint
    "LAKE FINNISS"
    (Just "NT")
    "LFS"
    (Lat (-12) 22.8)
    (Lon 131 28.5)

_LFRO_ ::
  VFR_Waypoint
_LFRO_ =
  VFR_Waypoint
    "LAKE FROME"
    (Just "SA")
    "LFRO"
    (Lat (-30) 38.0)
    (Lon 139 52.0)

_LGGN_ ::
  VFR_Waypoint
_LGGN_ =
  VFR_Waypoint
    "LAKE GEORGE NORTH"
    (Just "NSW")
    "LGGN"
    (Lat (-34) 59.3)
    (Lon 149 23.5)

_LGGS_ ::
  VFR_Waypoint
_LGGS_ =
  VFR_Waypoint
    "LAKE GEORGE SOUTH"
    (Just "NSW")
    "LGGS"
    (Lat (-35) 12.2)
    (Lon 149 24.5)

_LGEN_ ::
  VFR_Waypoint
_LGEN_ =
  VFR_Waypoint
    "LAKE GILLEN"
    (Just "WA")
    "LGEN"
    (Lat (-26) 13.0)
    (Lon 124 36.0)

_LGDA_ ::
  VFR_Waypoint
_LGDA_ =
  VFR_Waypoint
    "LAKE GININDERRA"
    (Just "ACT")
    "LGDA"
    (Lat (-35) 14.0)
    (Lon 149 4.1)

_LKH_ ::
  VFR_Waypoint
_LKH_ =
  VFR_Waypoint
    "LAKE HINDMARSH"
    (Just "VIC")
    "LKH"
    (Lat (-36) 7.0)
    (Lon 141 52.0)

_LKIM_ ::
  VFR_Waypoint
_LKIM_ =
  VFR_Waypoint
    "LAKE ILMA"
    (Just "WA")
    "LKIM"
    (Lat (-29) 15.0)
    (Lon 127 46.0)

_LKG_ ::
  VFR_Waypoint
_LKG_ =
  VFR_Waypoint
    "LAKE KING"
    (Just "WA")
    "LKG"
    (Lat (-33) 5.0)
    (Lon 119 40.0)

_LMC_ ::
  VFR_Waypoint
_LMC_ =
  VFR_Waypoint
    "LAKE MANCHESTER"
    (Just "QLD")
    "LMC"
    (Lat (-27) 29.0)
    (Lon 152 46.0)

_LME_ ::
  VFR_Waypoint
_LME_ =
  VFR_Waypoint
    "LAKE MAURICE"
    (Just "SA")
    "LME"
    (Lat (-29) 24.0)
    (Lon 130 57.0)

_LMWL_ ::
  VFR_Waypoint
_LMWL_ =
  VFR_Waypoint
    "LAKE MINIGWAL"
    (Just "WA")
    "LMWL"
    (Lat (-29) 35.0)
    (Lon 123 10.0)

_LOOH_ ::
  VFR_Waypoint
_LOOH_ =
  VFR_Waypoint
    "LAKE MOOGERAH"
    (Just "QLD")
    "LOOH"
    (Lat (-28) 2.0)
    (Lon 152 33.0)

_LKNL_ ::
  VFR_Waypoint
_LKNL_ =
  VFR_Waypoint
    "LAKE NEALE"
    (Just "NT")
    "LKNL"
    (Lat (-24) 14.8)
    (Lon 129 58.0)

_LRID_ ::
  VFR_Waypoint
_LRID_ =
  VFR_Waypoint
    "LAKE RAESIDE"
    (Just "WA")
    "LRID"
    (Lat (-29) 33.0)
    (Lon 122 18.0)

_LRAN_ ::
  VFR_Waypoint
_LRAN_ =
  VFR_Waypoint
    "LAKE RASON"
    (Just "WA")
    "LRAN"
    (Lat (-28) 40.0)
    (Lon 124 17.0)

_LSPR_ ::
  VFR_Waypoint
_LSPR_ =
  VFR_Waypoint
    "LAKE SURPRISE"
    (Just "NT")
    "LSPR"
    (Lat (-20) 14.0)
    (Lon 131 48.0)

_LTOM_ ::
  VFR_Waypoint
_LTOM_ =
  VFR_Waypoint
    "LAKE THOMSON"
    (Just "WA")
    "LTOM"
    (Lat (-32) 9.1)
    (Lon 115 50.1)

_LTL_ ::
  VFR_Waypoint
_LTL_ =
  VFR_Waypoint
    "LAKE THROSSELL"
    (Just "WA")
    "LTL"
    (Lat (-27) 38.0)
    (Lon 124 5.0)

_LTRR_ ::
  VFR_Waypoint
_LTRR_ =
  VFR_Waypoint
    "LAKE TORRENS"
    (Just "SA")
    "LTRR"
    (Lat (-31) 25.3)
    (Lon 138 5.2)

_WITE_ ::
  VFR_Waypoint
_WITE_ =
  VFR_Waypoint
    "LAKE WHITE"
    (Just "NT")
    "WITE"
    (Lat (-21) 6.0)
    (Lon 129 3.0)

_LYEO_ ::
  VFR_Waypoint
_LYEO_ =
  VFR_Waypoint
    "LAKE YEO"
    (Just "WA")
    "LYEO"
    (Lat (-28) 3.1)
    (Lon 124 33.1)

_LKEE_ ::
  VFR_Waypoint
_LKEE_ =
  VFR_Waypoint
    "LAKES ENTRANCE"
    (Just "VIC")
    "LKEE"
    (Lat (-37) 52.0)
    (Lon 148 0.0)

_LCD_ ::
  VFR_Waypoint
_LCD_ =
  VFR_Waypoint
    "LANCEFIELD"
    (Just "VIC")
    "LCD"
    (Lat (-37) 16.7)
    (Lon 144 43.7)

_LANC_ ::
  VFR_Waypoint
_LANC_ =
  VFR_Waypoint
    "LANCELIN TOWNSHIP"
    (Just "WA")
    "LANC"
    (Lat (-31) 1.1)
    (Lon 115 19.7)

_LGH_ ::
  VFR_Waypoint
_LGH_ =
  VFR_Waypoint
    "LANGHAM"
    (Just "QLD")
    "LGH"
    (Lat (-22) 12.0)
    (Lon 150 6.0)

_LHCK_ ::
  VFR_Waypoint
_LHCK_ =
  VFR_Waypoint
    "LANGHORNE CREEK"
    (Just "SA")
    "LHCK"
    (Lat (-35) 17.8)
    (Lon 139 2.7)

_LSDW_ ::
  VFR_Waypoint
_LSDW_ =
  VFR_Waypoint
    "LANSDOWNE"
    (Just "WA")
    "LSDW"
    (Lat (-17) 36.9)
    (Lon 126 44.6)

_LUY_ ::
  VFR_Waypoint
_LUY_ =
  VFR_Waypoint
    "LATROBE UNIVERSITY"
    (Just "VIC")
    "LUY"
    (Lat (-37) 43.0)
    (Lon 145 3.0)

_LAUD_ ::
  VFR_Waypoint
_LAUD_ =
  VFR_Waypoint
    "LAUDERDALE"
    (Just "TAS")
    "LAUD"
    (Lat (-42) 54.0)
    (Lon 147 30.0)

_LVAB_ ::
  VFR_Waypoint
_LVAB_ =
  VFR_Waypoint
    "LAVERTON"
    (Just "VIC")
    "LVAB"
    (Lat (-37) 51.8)
    (Lon 144 44.7)

_TON_ ::
  VFR_Waypoint
_TON_ =
  VFR_Waypoint
    "LAVERTON BOM TOWER"
    (Just "VIC")
    "TON"
    (Lat (-37) 51.3)
    (Lon 144 45.3)

_LWG_ ::
  VFR_Waypoint
_LWG_ =
  VFR_Waypoint
    "LAWRENCE GORGE"
    (Just "NT")
    "LWG"
    (Lat (-24) 1.0)
    (Lon 133 24.5)

_LAOS_ ::
  VFR_Waypoint
_LAOS_ =
  VFR_Waypoint
    "LAYOAK ISLAND"
    (Just "QLD")
    "LAOS"
    (Lat (-9) 51.0)
    (Lon 143 19.0)

_LPT_ ::
  VFR_Waypoint
_LPT_ =
  VFR_Waypoint
    "LEE PT"
    (Just "NT")
    "LPT"
    (Lat (-12) 20.0)
    (Lon 130 54.0)

_LPD_ ::
  VFR_Waypoint
_LPD_ =
  VFR_Waypoint
    "LEOPOLD"
    (Just "VIC")
    "LPD"
    (Lat (-38) 11.5)
    (Lon 144 28.0)

_LIHR_ ::
  VFR_Waypoint
_LIHR_ =
  VFR_Waypoint
    "LIGHTHORSE INTERCHANGE M7/M4"
    (Just "NSW")
    "LIHR"
    (Lat (-33) 47.9)
    (Lon 150 51.2)

_LIY_ ::
  VFR_Waypoint
_LIY_ =
  VFR_Waypoint
    "LILYDALE"
    (Just "TAS")
    "LIY"
    (Lat (-41) 15.0)
    (Lon 147 13.0)

_LOWS_ ::
  VFR_Waypoint
_LOWS_ =
  VFR_Waypoint
    "LINDENOW SOUTH"
    (Just "VIC")
    "LOWS"
    (Lat (-37) 49.7)
    (Lon 147 26.0)

_LVE_ ::
  VFR_Waypoint
_LVE_ =
  VFR_Waypoint
    "LINVILLE"
    (Just "QLD")
    "LVE"
    (Lat (-26) 51.0)
    (Lon 152 16.0)

_LMGE_ ::
  VFR_Waypoint
_LMGE_ =
  VFR_Waypoint
    "LITTLE MULGRAVE"
    (Just "QLD")
    "LMGE"
    (Lat (-17) 8.3)
    (Lon 145 43.5)

_LRM_ ::
  VFR_Waypoint
_LRM_ =
  VFR_Waypoint
    "LITTLE RIVER MOUTH"
    (Just "VIC")
    "LRM"
    (Lat (-38) 0.4)
    (Lon 144 35.0)

_LBET_ ::
  VFR_Waypoint
_LBET_ =
  VFR_Waypoint
    "LOBETHAL"
    (Just "SA")
    "LBET"
    (Lat (-34) 54.0)
    (Lon 138 52.0)

_LSR_ ::
  VFR_Waypoint
_LSR_ =
  VFR_Waypoint
    "LOCH SPORT"
    (Just "VIC")
    "LSR"
    (Lat (-38) 3.0)
    (Lon 147 35.0)

_LOGI_ ::
  VFR_Waypoint
_LOGI_ =
  VFR_Waypoint
    "LOGIC CENTRE"
    (Just "VIC")
    "LOGI"
    (Lat (-36) 4.6)
    (Lon 146 43.1)

_LRF_ ::
  VFR_Waypoint
_LRF_ =
  VFR_Waypoint
    "LONG REEF"
    (Just "NSW")
    "LRF"
    (Lat (-33) 44.5)
    (Lon 151 19.3)

_LFC_ ::
  VFR_Waypoint
_LFC_ =
  VFR_Waypoint
    "LONGFORD CREEK"
    (Just "QLD")
    "LFC"
    (Lat (-20) 12.5)
    (Lon 148 22.0)

_LORN_ ::
  VFR_Waypoint
_LORN_ =
  VFR_Waypoint
    "LORNE TOWNSHIP"
    (Just "VIC")
    "LORN"
    (Lat (-38) 32.6)
    (Lon 143 58.2)

_LHD_ ::
  VFR_Waypoint
_LHD_ =
  VFR_Waypoint
    "LOW HEAD"
    (Just "TAS")
    "LHD"
    (Lat (-41) 4.0)
    (Lon 146 48.0)

_LWI_ ::
  VFR_Waypoint
_LWI_ =
  VFR_Waypoint
    "LOW ISLETS"
    (Just "QLD")
    "LWI"
    (Lat (-16) 23.0)
    (Lon 145 34.0)

_LRP_ ::
  VFR_Waypoint
_LRP_ =
  VFR_Waypoint
    "LOW ROCKY PT"
    (Just "TAS")
    "LRP"
    (Lat (-43) 0.0)
    (Lon 145 30.0)

_LWD_ ::
  VFR_Waypoint
_LWD_ =
  VFR_Waypoint
    "LOWOOD"
    (Just "QLD")
    "LWD"
    (Lat (-27) 28.0)
    (Lon 152 35.0)

_LNDA_ ::
  VFR_Waypoint
_LNDA_ =
  VFR_Waypoint
    "LUCINDA"
    (Just "QLD")
    "LNDA"
    (Lat (-18) 32.0)
    (Lon 146 20.0)

_LYNR_ ::
  VFR_Waypoint
_LYNR_ =
  VFR_Waypoint
    "LYND RIVER"
    (Just "QLD")
    "LYNR"
    (Lat (-17) 24.0)
    (Lon 143 45.0)

_MACB_ ::
  VFR_Waypoint
_MACB_ =
  VFR_Waypoint
    "MACHANS BEACH"
    (Just "QLD")
    "MACB"
    (Lat (-16) 51.0)
    (Lon 145 45.0)

_MZR_ ::
  VFR_Waypoint
_MZR_ =
  VFR_Waypoint
    "MACKENZIE RIVER"
    (Just "QLD")
    "MZR"
    (Lat (-23) 10.0)
    (Lon 149 30.0)

_MKV_ ::
  VFR_Waypoint
_MKV_ =
  VFR_Waypoint
    "MACKSVILLE"
    (Just "NSW")
    "MKV"
    (Lat (-30) 42.5)
    (Lon 152 54.9)

_MGEW_ ::
  VFR_Waypoint
_MGEW_ =
  VFR_Waypoint
    "MAGILL ESTATE WINERY"
    (Just "SA")
    "MGEW"
    (Lat (-34) 55.2)
    (Lon 138 40.8)

_MAS_ ::
  VFR_Waypoint
_MAS_ =
  VFR_Waypoint
    "MAGNESITE MINE"
    (Just "QLD")
    "MAS"
    (Lat (-22) 53.3)
    (Lon 150 11.0)

_MADO_ ::
  VFR_Waypoint
_MADO_ =
  VFR_Waypoint
    "MANDORAH"
    (Just "NT")
    "MADO"
    (Lat (-12) 26.5)
    (Lon 130 45.7)

_MDU_ ::
  VFR_Waypoint
_MDU_ =
  VFR_Waypoint
    "MANDURAH"
    (Just "WA")
    "MDU"
    (Lat (-32) 31.8)
    (Lon 115 43.3)

_MGL_ ::
  VFR_Waypoint
_MGL_ =
  VFR_Waypoint
    "MANGALORE"
    (Just "TAS")
    "MGL"
    (Lat (-42) 39.5)
    (Lon 147 14.5)

_MOP_ ::
  VFR_Waypoint
_MOP_ =
  VFR_Waypoint
    "MANGOPLAH"
    (Just "NSW")
    "MOP"
    (Lat (-35) 23.5)
    (Lon 147 14.5)

_MAL_ ::
  VFR_Waypoint
_MAL_ =
  VFR_Waypoint
    "MANILLA"
    (Just "NSW")
    "MAL"
    (Lat (-30) 45.0)
    (Lon 150 43.4)

_MANLY_ ::
  VFR_Waypoint
_MANLY_ =
  VFR_Waypoint
    "MANLY"
    (Just "NSW")
    "MANLY"
    (Lat (-33) 47.9)
    (Lon 151 17.3)

_MTD_ ::
  VFR_Waypoint
_MTD_ =
  VFR_Waypoint
    "MANTON DAM"
    (Just "NT")
    "MTD"
    (Lat (-12) 51.0)
    (Lon 131 7.5)

_MBA_ ::
  VFR_Waypoint
_MBA_ =
  VFR_Waypoint
    "MAREEBA"
    (Just "QLD")
    "MBA"
    (Lat (-17) 4.1)
    (Lon 145 25.1)

_MRL_ ::
  VFR_Waypoint
_MRL_ =
  VFR_Waypoint
    "MARIA ISLAND"
    (Just "TAS")
    "MRL"
    (Lat (-42) 38.0)
    (Lon 148 5.0)

_MLIT_ ::
  VFR_Waypoint
_MLIT_ =
  VFR_Waypoint
    "MARINO LIGHT HOUSE"
    (Just "SA")
    "MLIT"
    (Lat (-35) 3.3)
    (Lon 138 30.8)

_MARQ_ ::
  VFR_Waypoint
_MARQ_ =
  VFR_Waypoint
    "MARINO'S QUARRY"
    (Just "QLD")
    "MARQ"
    (Lat (-16) 55.2)
    (Lon 145 43.6)

_MRBR_ ::
  VFR_Waypoint
_MRBR_ =
  VFR_Waypoint
    "MAROUBRA BEACH"
    (Just "NSW")
    "MRBR"
    (Lat (-33) 57.0)
    (Lon 151 15.4)

_MAR_ ::
  VFR_Waypoint
_MAR_ =
  VFR_Waypoint
    "MARRAR"
    (Just "NSW")
    "MAR"
    (Lat (-34) 49.5)
    (Lon 147 21.0)

_MARR_ ::
  VFR_Waypoint
_MARR_ =
  VFR_Waypoint
    "MARRAWAH"
    (Just "TAS")
    "MARR"
    (Lat (-40) 54.6)
    (Lon 144 42.5)

_MHT_ ::
  VFR_Waypoint
_MHT_ =
  VFR_Waypoint
    "MARTHA PT"
    (Just "VIC")
    "MHT"
    (Lat (-38) 19.0)
    (Lon 144 59.0)

_MKN_ ::
  VFR_Waypoint
_MKN_ =
  VFR_Waypoint
    "MARY KATHLEEN"
    (Just "QLD")
    "MKN"
    (Lat (-20) 47.0)
    (Lon 139 59.0)

_MVL_ ::
  VFR_Waypoint
_MVL_ =
  VFR_Waypoint
    "MARYSVILLE"
    (Just "VIC")
    "MVL"
    (Lat (-37) 31.0)
    (Lon 145 45.0)

_MHD_ ::
  VFR_Waypoint
_MHD_ =
  VFR_Waypoint
    "MASTHEAD ISLAND"
    (Just "QLD")
    "MHD"
    (Lat (-23) 32.0)
    (Lon 151 44.0)

_MRH_ ::
  VFR_Waypoint
_MRH_ =
  VFR_Waypoint
    "MAURICE HILL"
    (Just "QLD")
    "MRH"
    (Lat (-23) 56.0)
    (Lon 151 15.0)

_MAYD_ ::
  VFR_Waypoint
_MAYD_ =
  VFR_Waypoint
    "MAYDENA"
    (Just "TAS")
    "MAYD"
    (Lat (-42) 45.8)
    (Lon 146 35.8)

_MYF_ ::
  VFR_Waypoint
_MYF_ =
  VFR_Waypoint
    "MAYFIELD"
    (Just "NSW")
    "MYF"
    (Lat (-33) 57.8)
    (Lon 150 37.5)

_MAYL_ ::
  VFR_Waypoint
_MAYL_ =
  VFR_Waypoint
    "MAYLANDS POLICE ACADEMY"
    (Just "WA")
    "MAYL"
    (Lat (-31) 56.8)
    (Lon 115 54.2)

_MVAL_ ::
  VFR_Waypoint
_MVAL_ =
  VFR_Waypoint
    "MCLAREN VALE"
    (Just "SA")
    "MVAL"
    (Lat (-35) 13.5)
    (Lon 138 32.8)

_MCTY_ ::
  VFR_Waypoint
_MCTY_ =
  VFR_Waypoint
    "MELBOURNE CBD"
    (Just "VIC")
    "MCTY"
    (Lat (-37) 48.5)
    (Lon 144 57.5)

_MCG_ ::
  VFR_Waypoint
_MCG_ =
  VFR_Waypoint
    "MELBOURNE CRICKET GROUND"
    (Just "VIC")
    "MCG"
    (Lat (-37) 49.2)
    (Lon 144 59.0)

_MELS_ ::
  VFR_Waypoint
_MELS_ =
  VFR_Waypoint
    "MELTON SOUTH"
    (Just "VIC")
    "MELS"
    (Lat (-37) 41.0)
    (Lon 144 34.0)

_MEG_ ::
  VFR_Waypoint
_MEG_ =
  VFR_Waypoint
    "MENANGLE"
    (Just "NSW")
    "MEG"
    (Lat (-34) 7.5)
    (Lon 150 44.5)

_MRI_ ::
  VFR_Waypoint
_MRI_ =
  VFR_Waypoint
    "MERION"
    (Just "QLD")
    "MRI"
    (Lat (-22) 52.0)
    (Lon 149 2.0)

_MRJ_ ::
  VFR_Waypoint
_MRJ_ =
  VFR_Waypoint
    "MERRIJIG"
    (Just "VIC")
    "MRJ"
    (Lat (-37) 7.0)
    (Lon 146 15.0)

_MIAR_ ::
  VFR_Waypoint
_MIAR_ =
  VFR_Waypoint
    "MIAREE POOL BRIDGE"
    (Just "WA")
    "MIAR"
    (Lat (-20) 51.0)
    (Lon 116 36.6)

_MHGO_ ::
  VFR_Waypoint
_MHGO_ =
  VFR_Waypoint
    "MICHELAGO"
    (Just "NSW")
    "MHGO"
    (Lat (-35) 43.0)
    (Lon 149 10.0)

_MII_ ::
  VFR_Waypoint
_MII_ =
  VFR_Waypoint
    "MIDGE ISLAND"
    (Just "QLD")
    "MII"
    (Lat (-20) 41.5)
    (Lon 148 45.7)

_MCRO_ ::
  VFR_Waypoint
_MCRO_ =
  VFR_Waypoint
    "MILLS CROSS"
    (Just "NSW")
    "MCRO"
    (Lat (-35) 22.3)
    (Lon 149 25.5)

_MSTM_ ::
  VFR_Waypoint
_MSTM_ =
  VFR_Waypoint
    "MILLSTREAM STN"
    (Just "WA")
    "MSTM"
    (Lat (-21) 37.0)
    (Lon 117 4.0)

_MPO_ ::
  VFR_Waypoint
_MPO_ =
  VFR_Waypoint
    "MILPEROO"
    (Just "QLD")
    "MPO"
    (Lat (-23) 11.0)
    (Lon 141 15.0)

_MIJ_ ::
  VFR_Waypoint
_MIJ_ =
  VFR_Waypoint
    "MINJILANG"
    (Just "NT")
    "MIJ"
    (Lat (-11) 8.9)
    (Lon 132 34.8)

_MISB_ ::
  VFR_Waypoint
_MISB_ =
  VFR_Waypoint
    "MISSION BEACH"
    (Just "QLD")
    "MISB"
    (Lat (-17) 52.2)
    (Lon 146 6.4)

_MSC_ ::
  VFR_Waypoint
_MSC_ =
  VFR_Waypoint
    "MISSION BEACH"
    (Just "QLD")
    "MSC"
    (Lat (-17) 53.0)
    (Lon 146 6.0)

_MITI_ ::
  VFR_Waypoint
_MITI_ =
  VFR_Waypoint
    "MITIAMO"
    (Just "VIC")
    "MITI"
    (Lat (-36) 12.7)
    (Lon 144 13.8)

_MFBH_ ::
  VFR_Waypoint
_MFBH_ =
  VFR_Waypoint
    "MOFFAT BEACH"
    (Just "QLD")
    "MFBH"
    (Lat (-26) 47.3)
    (Lon 153 8.5)

_MFH_ ::
  VFR_Waypoint
_MFH_ =
  VFR_Waypoint
    "MOFFAT HEAD"
    (Just "QLD")
    "MFH"
    (Lat (-26) 47.4)
    (Lon 153 8.4)

_MBH_ ::
  VFR_Waypoint
_MBH_ =
  VFR_Waypoint
    "MOOLOOLABA"
    (Just "QLD")
    "MBH"
    (Lat (-26) 42.0)
    (Lon 153 8.0)

_MVC_ ::
  VFR_Waypoint
_MVC_ =
  VFR_Waypoint
    "MOONEE VALLEY RACECOURSE"
    (Just "VIC")
    "MVC"
    (Lat (-37) 46.0)
    (Lon 144 56.0)

_MIE_ ::
  VFR_Waypoint
_MIE_ =
  VFR_Waypoint
    "MOONIE"
    (Just "QLD")
    "MIE"
    (Lat (-27) 43.0)
    (Lon 150 22.0)

_MPSC_ ::
  VFR_Waypoint
_MPSC_ =
  VFR_Waypoint
    "MOORE PARK SUPA CENTRE"
    (Just "NSW")
    "MPSC"
    (Lat (-33) 54.2)
    (Lon 151 12.9)

_MORN_ ::
  VFR_Waypoint
_MORN_ =
  VFR_Waypoint
    "MORNINGTON"
    (Just "TAS")
    "MORN"
    (Lat (-42) 51.4)
    (Lon 147 24.7)

_MSV_ ::
  VFR_Waypoint
_MSV_ =
  VFR_Waypoint
    "MOSS VALE"
    (Just "NSW")
    "MSV"
    (Lat (-34) 31.5)
    (Lon 150 25.3)

_MMA_ ::
  VFR_Waypoint
_MMA_ =
  VFR_Waypoint
    "MOSSMAN"
    (Just "QLD")
    "MMA"
    (Lat (-16) 27.5)
    (Lon 145 22.3)

_BAK_ ::
  VFR_Waypoint
_BAK_ =
  VFR_Waypoint
    "MOUNT BARKER"
    (Just "SA")
    "BAK"
    (Lat (-35) 4.7)
    (Lon 138 52.0)

_MBST_ ::
  VFR_Waypoint
_MBST_ =
  VFR_Waypoint
    "MOUNT BENSTEAD"
    (Just "NT")
    "MBST"
    (Lat (-23) 34.0)
    (Lon 134 16.5)

_MBC_ ::
  VFR_Waypoint
_MBC_ =
  VFR_Waypoint
    "MOUNT BLACK"
    (Just "QLD")
    "MBC"
    (Lat (-19) 17.0)
    (Lon 146 33.5)

_MBK_ ::
  VFR_Waypoint
_MBK_ =
  VFR_Waypoint
    "MOUNT BLACKWOOD"
    (Just "QLD")
    "MBK"
    (Lat (-21) 2.0)
    (Lon 148 56.5)

_MTBL_ ::
  VFR_Waypoint
_MTBL_ =
  VFR_Waypoint
    "MOUNT BOHLE"
    (Just "QLD")
    "MTBL"
    (Lat (-19) 16.0)
    (Lon 146 41.3)

_MBR_ ::
  VFR_Waypoint
_MBR_ =
  VFR_Waypoint
    "MOUNT BOLD RESV"
    (Just "SA")
    "MBR"
    (Lat (-35) 7.3)
    (Lon 138 42.0)

_MTB_ ::
  VFR_Waypoint
_MTB_ =
  VFR_Waypoint
    "MOUNT BOYCE"
    (Just "NSW")
    "MTB"
    (Lat (-33) 37.1)
    (Lon 150 16.4)

_MCAR_ ::
  VFR_Waypoint
_MCAR_ =
  VFR_Waypoint
    "MOUNT CARNARVON"
    (Just "QLD")
    "MCAR"
    (Lat (-24) 55.0)
    (Lon 148 23.0)

_MCHR_ ::
  VFR_Waypoint
_MCHR_ =
  VFR_Waypoint
    "MOUNT CHRISTIE"
    (Just "SA")
    "MCHR"
    (Lat (-30) 33.0)
    (Lon 133 13.0)

_MCOM_ ::
  VFR_Waypoint
_MCOM_ =
  VFR_Waypoint
    "MOUNT COMPASS"
    (Just "SA")
    "MCOM"
    (Lat (-35) 21.0)
    (Lon 138 37.0)

_MTK_ ::
  VFR_Waypoint
_MTK_ =
  VFR_Waypoint
    "MOUNT COOKE"
    (Just "WA")
    "MTK"
    (Lat (-32) 25.0)
    (Lon 116 18.4)

_MTC_ ::
  VFR_Waypoint
_MTC_ =
  VFR_Waypoint
    "MOUNT COOLUM"
    (Just "QLD")
    "MTC"
    (Lat (-26) 33.7)
    (Lon 153 5.0)

_MTBA_ ::
  VFR_Waypoint
_MTBA_ =
  VFR_Waypoint
    "MOUNT CORAMBA"
    (Just "NSW")
    "MTBA"
    (Lat (-30) 13.3)
    (Lon 153 3.0)

_TCR_ ::
  VFR_Waypoint
_TCR_ =
  VFR_Waypoint
    "MOUNT COREE"
    (Just "ACT")
    "TCR"
    (Lat (-35) 18.5)
    (Lon 148 48.6)

_MCOO_ ::
  VFR_Waypoint
_MCOO_ =
  VFR_Waypoint
    "MOUNT COTTON"
    (Just "QLD")
    "MCOO"
    (Lat (-27) 37.3)
    (Lon 153 13.0)

_MCOT_ ::
  VFR_Waypoint
_MCOT_ =
  VFR_Waypoint
    "MOUNT COTTRELL"
    (Just "VIC")
    "MCOT"
    (Lat (-37) 45.8)
    (Lon 144 37.2)

_MUE_ ::
  VFR_Waypoint
_MUE_ =
  VFR_Waypoint
    "MOUNT DALE"
    (Just "WA")
    "MUE"
    (Lat (-32) 7.6)
    (Lon 116 17.8)

_MDY_ ::
  VFR_Waypoint
_MDY_ =
  VFR_Waypoint
    "MOUNT DAY"
    (Just "WA")
    "MDY"
    (Lat (-32) 8.0)
    (Lon 120 30.0)

_MELE_ ::
  VFR_Waypoint
_MELE_ =
  VFR_Waypoint
    "MOUNT ELEPHANT"
    (Just "QLD")
    "MELE"
    (Lat (-16) 27.0)
    (Lon 144 56.0)

_MEV_ ::
  VFR_Waypoint
_MEV_ =
  VFR_Waypoint
    "MOUNT EVERARD"
    (Just "WA")
    "MEV"
    (Lat (-25) 11.0)
    (Lon 125 4.0)

_MFN_ ::
  VFR_Waypoint
_MFN_ =
  VFR_Waypoint
    "MOUNT FUNNEL"
    (Just "QLD")
    "MFN"
    (Lat (-21) 37.5)
    (Lon 149 23.2)

_MGLO_ ::
  VFR_Waypoint
_MGLO_ =
  VFR_Waypoint
    "MOUNT GLORIOUS"
    (Just "QLD")
    "MGLO"
    (Lat (-27) 20.0)
    (Lon 152 46.0)

_MVT_ ::
  VFR_Waypoint
_MVT_ =
  VFR_Waypoint
    "MOUNT GRAVATT"
    (Just "QLD")
    "MVT"
    (Lat (-27) 33.0)
    (Lon 153 4.5)

_MJK_ ::
  VFR_Waypoint
_MJK_ =
  VFR_Waypoint
    "MOUNT JACKSON HS"
    (Just "WA")
    "MJK"
    (Lat (-30) 12.0)
    (Lon 119 6.0)

_MTKI_ ::
  VFR_Waypoint
_MTKI_ =
  VFR_Waypoint
    "MOUNT KINGSTON"
    (Just "NT")
    "MTKI"
    (Lat (-25) 26.0)
    (Lon 133 38.0)

_MLI_ ::
  VFR_Waypoint
_MLI_ =
  VFR_Waypoint
    "MOUNT LION"
    (Just "QLD")
    "MLI"
    (Lat (-23) 23.5)
    (Lon 150 19.3)

_MLUY_ ::
  VFR_Waypoint
_MLUY_ =
  VFR_Waypoint
    "MOUNT LUCY"
    (Just "NT")
    "MLUY"
    (Lat (-22) 36.0)
    (Lon 133 32.0)

_MCD_ ::
  VFR_Waypoint
_MCD_ =
  VFR_Waypoint
    "MOUNT MACEDON"
    (Just "VIC")
    "MCD"
    (Lat (-37) 22.5)
    (Lon 144 34.6)

_MTMA_ ::
  VFR_Waypoint
_MTMA_ =
  VFR_Waypoint
    "MOUNT MARIA"
    (Just "QLD")
    "MTMA"
    (Lat (-27) 28.0)
    (Lon 151 29.0)

_MMY_ ::
  VFR_Waypoint
_MMY_ =
  VFR_Waypoint
    "MOUNT MOLLOY"
    (Just "QLD")
    "MMY"
    (Lat (-16) 40.7)
    (Lon 145 19.8)

_MOB_ ::
  VFR_Waypoint
_MOB_ =
  VFR_Waypoint
    "MOUNT MOOMBIL"
    (Just "NSW")
    "MOB"
    (Lat (-30) 18.9)
    (Lon 152 51.1)

_MGN_ ::
  VFR_Waypoint
_MGN_ =
  VFR_Waypoint
    "MOUNT MORGAN"
    (Just "QLD")
    "MGN"
    (Lat (-23) 38.7)
    (Lon 150 23.5)

_MTM_ ::
  VFR_Waypoint
_MTM_ =
  VFR_Waypoint
    "MOUNT MUGGA"
    (Just "ACT")
    "MTM"
    (Lat (-35) 21.3)
    (Lon 149 7.8)

_MIY_ ::
  VFR_Waypoint
_MIY_ =
  VFR_Waypoint
    "MOUNT NINDERRY"
    (Just "QLD")
    "MIY"
    (Lat (-26) 33.3)
    (Lon 152 59.5)

_OOR_ ::
  VFR_Waypoint
_OOR_ =
  VFR_Waypoint
    "MOUNT OORAMINNA"
    (Just "NT")
    "OOR"
    (Lat (-24) 5.5)
    (Lon 134 0.2)

_MPG_ ::
  VFR_Waypoint
_MPG_ =
  VFR_Waypoint
    "MOUNT PALERANG"
    (Just "NSW")
    "MPG"
    (Lat (-35) 26.0)
    (Lon 149 36.0)

_PIPR_ ::
  VFR_Waypoint
_PIPR_ =
  VFR_Waypoint
    "MOUNT PIPER"
    (Just "VIC")
    "PIPR"
    (Lat (-37) 12.3)
    (Lon 145 0.2)

_MPT_ ::
  VFR_Waypoint
_MPT_ =
  VFR_Waypoint
    "MOUNT PLEASANT"
    (Just "SA")
    "MPT"
    (Lat (-34) 46.5)
    (Lon 139 3.0)

_SOV_ ::
  VFR_Waypoint
_SOV_ =
  VFR_Waypoint
    "MOUNT SOMERVILLE RADAR"
    (Just "NSW")
    "SOV"
    (Lat (-28) 12.9)
    (Lon 153 25.6)

_MTEW_ ::
  VFR_Waypoint
_MTEW_ =
  VFR_Waypoint
    "MOUNT STEWAN"
    (Just "QLD")
    "MTEW"
    (Lat (-20) 22.0)
    (Lon 144 3.0)

_MUM_ ::
  VFR_Waypoint
_MUM_ =
  VFR_Waypoint
    "MOUNT STROMLO"
    (Just "ACT")
    "MUM"
    (Lat (-35) 19.0)
    (Lon 149 0.5)

_MUSD_ ::
  VFR_Waypoint
_MUSD_ =
  VFR_Waypoint
    "MOUNT SYDNEY"
    (Just "WA")
    "MUSD"
    (Lat (-21) 24.0)
    (Lon 121 12.0)

_MTLR_ ::
  VFR_Waypoint
_MTLR_ =
  VFR_Waypoint
    "MOUNT TAYLOR"
    (Just "ACT")
    "MTLR"
    (Lat (-35) 22.4)
    (Lon 149 4.6)

_MTY_ ::
  VFR_Waypoint
_MTY_ =
  VFR_Waypoint
    "MOUNT TYSON"
    (Just "QLD")
    "MTY"
    (Lat (-27) 35.0)
    (Lon 151 34.0)

_MUO_ ::
  VFR_Waypoint
_MUO_ =
  VFR_Waypoint
    "MOUNT UNDOOLYA"
    (Just "NT")
    "MUO"
    (Lat (-23) 44.3)
    (Lon 134 6.2)

_MVO_ ::
  VFR_Waypoint
_MVO_ =
  VFR_Waypoint
    "MOUNT VERNON HS"
    (Just "WA")
    "MVO"
    (Lat (-24) 14.0)
    (Lon 118 14.0)

_MVI_ ::
  VFR_Waypoint
_MVI_ =
  VFR_Waypoint
    "MOUNT VICTORIA"
    (Just "NSW")
    "MVI"
    (Lat (-33) 35.0)
    (Lon 150 15.0)

_MTWK_ ::
  VFR_Waypoint
_MTWK_ =
  VFR_Waypoint
    "MOUNT WALKER"
    (Just "QLD")
    "MTWK"
    (Lat (-27) 47.3)
    (Lon 152 33.4)

_MTWG_ ::
  VFR_Waypoint
_MTWG_ =
  VFR_Waypoint
    "MOUNT WARNING"
    (Just "NSW")
    "MTWG"
    (Lat (-28) 24.0)
    (Lon 153 16.0)

_MTWN_ ::
  VFR_Waypoint
_MTWN_ =
  VFR_Waypoint
    "MOUNT WELLINGTON"
    (Just "VIC")
    "MTWN"
    (Lat (-37) 33.6)
    (Lon 146 48.6)

_MWH_ ::
  VFR_Waypoint
_MWH_ =
  VFR_Waypoint
    "MOUNT WHEELER"
    (Just "QLD")
    "MWH"
    (Lat (-23) 13.6)
    (Lon 150 41.0)

_MWK_ ::
  VFR_Waypoint
_MWK_ =
  VFR_Waypoint
    "MOUNT WILKIE"
    (Just "WA")
    "MWK"
    (Lat (-20) 57.1)
    (Lon 116 25.1)

_MBKR_ ::
  VFR_Waypoint
_MBKR_ =
  VFR_Waypoint
    "MOUTH OF THE BLACK RIVER"
    (Just "QLD")
    "MBKR"
    (Lat (-19) 10.8)
    (Lon 146 39.2)

_MBHR_ ::
  VFR_Waypoint
_MBHR_ =
  VFR_Waypoint
    "MOUTH OF THE BOHLE RIVER"
    (Just "QLD")
    "MBHR"
    (Lat (-19) 11.8)
    (Lon 146 42.1)

_MMT_ ::
  VFR_Waypoint
_MMT_ =
  VFR_Waypoint
    "MT MARGARET"
    (Just "QLD")
    "MMT"
    (Lat (-19) 21.0)
    (Lon 146 36.1)

_MUDI_ ::
  VFR_Waypoint
_MUDI_ =
  VFR_Waypoint
    "MUD ISLAND"
    (Just "QLD")
    "MUDI"
    (Lat (-27) 20.0)
    (Lon 153 15.0)

_MEER_ ::
  VFR_Waypoint
_MEER_ =
  VFR_Waypoint
    "MUDGEERABA"
    (Just "QLD")
    "MEER"
    (Lat (-28) 5.0)
    (Lon 153 22.0)

_MUP_ ::
  VFR_Waypoint
_MUP_ =
  VFR_Waypoint
    "MULLALOO PT"
    (Just "WA")
    "MUP"
    (Lat (-31) 48.5)
    (Lon 115 43.5)

_LLN_ ::
  VFR_Waypoint
_LLN_ =
  VFR_Waypoint
    "MULLEN"
    (Just "QLD")
    "LLN"
    (Lat (-25) 2.2)
    (Lon 153 0.0)

_MBBY_ ::
  VFR_Waypoint
_MBBY_ =
  VFR_Waypoint
    "MULLUMBIMBY"
    (Just "NSW")
    "MBBY"
    (Lat (-28) 33.0)
    (Lon 153 30.0)

_MWR_ ::
  VFR_Waypoint
_MWR_ =
  VFR_Waypoint
    "MUNDARING WEIR"
    (Just "WA")
    "MWR"
    (Lat (-31) 57.3)
    (Lon 116 9.5)

_MAA_ ::
  VFR_Waypoint
_MAA_ =
  VFR_Waypoint
    "MUNGALLALA"
    (Just "QLD")
    "MAA"
    (Lat (-26) 27.0)
    (Lon 147 33.0)

_MUNM_ ::
  VFR_Waypoint
_MUNM_ =
  VFR_Waypoint
    "MUNIGANEEN MT"
    (Just "QLD")
    "MUNM"
    (Lat (-27) 24.5)
    (Lon 151 52.5)

_MHY_ ::
  VFR_Waypoint
_MHY_ =
  VFR_Waypoint
    "MURPHY'S CREEK"
    (Just "QLD")
    "MHY"
    (Lat (-27) 27.8)
    (Lon 152 3.3)

_MBD_ ::
  VFR_Waypoint
_MBD_ =
  VFR_Waypoint
    "MURRAY BRIDGE"
    (Just "SA")
    "MBD"
    (Lat (-35) 9.0)
    (Lon 139 18.6)

_MYW_ ::
  VFR_Waypoint
_MYW_ =
  VFR_Waypoint
    "MURRAY DOWNS"
    (Just "QLD")
    "MYW"
    (Lat (-25) 2.0)
    (Lon 139 12.0)

_MMM_ ::
  VFR_Waypoint
_MMM_ =
  VFR_Waypoint
    "MURRUMBATEMAN"
    (Just "NSW")
    "MMM"
    (Lat (-34) 58.3)
    (Lon 149 1.5)

_MUI_ ::
  VFR_Waypoint
_MUI_ =
  VFR_Waypoint
    "MURRURUNDI"
    (Just "NSW")
    "MUI"
    (Lat (-31) 46.0)
    (Lon 150 50.0)

_MUR_ ::
  VFR_Waypoint
_MUR_ =
  VFR_Waypoint
    "MURWILLUMBAH"
    (Just "NSW")
    "MUR"
    (Lat (-28) 19.5)
    (Lon 153 23.8)

_MUEE_ ::
  VFR_Waypoint
_MUEE_ =
  VFR_Waypoint
    "MUTARNEE"
    (Just "QLD")
    "MUEE"
    (Lat (-18) 57.0)
    (Lon 146 18.0)

_MRTL_ ::
  VFR_Waypoint
_MRTL_ =
  VFR_Waypoint
    "MYRTLE"
    (Just "QLD")
    "MRTL"
    (Lat (-19) 42.0)
    (Lon 146 32.0)

_NMB_ ::
  VFR_Waypoint
_NMB_ =
  VFR_Waypoint
    "NAMBOUR"
    (Just "QLD")
    "NMB"
    (Lat (-26) 37.7)
    (Lon 152 57.5)

_NHS_ ::
  VFR_Waypoint
_NHS_ =
  VFR_Waypoint
    "NAMBUCCA HEADS"
    (Just "NSW")
    "NHS"
    (Lat (-30) 38.7)
    (Lon 153 0.5)

_NAA_ ::
  VFR_Waypoint
_NAA_ =
  VFR_Waypoint
    "NARA INLET"
    (Just "QLD")
    "NAA"
    (Lat (-20) 9.0)
    (Lon 148 54.0)

_NAMA_ ::
  VFR_Waypoint
_NAMA_ =
  VFR_Waypoint
    "NAROOMA"
    (Just "NSW")
    "NAMA"
    (Lat (-36) 12.0)
    (Lon 150 8.0)

_NRW_ ::
  VFR_Waypoint
_NRW_ =
  VFR_Waypoint
    "NARRE WARREN"
    (Just "VIC")
    "NRW"
    (Lat (-38) 1.0)
    (Lon 145 18.0)

_NOOG_ ::
  VFR_Waypoint
_NOOG_ =
  VFR_Waypoint
    "NARROOGAL"
    (Just "QLD")
    "NOOG"
    (Lat (-10) 15.0)
    (Lon 142 30.0)

_NKBO_ ::
  VFR_Waypoint
_NKBO_ =
  VFR_Waypoint
    "NECKARBOO"
    (Just "NSW")
    "NKBO"
    (Lat (-32) 4.0)
    (Lon 144 37.0)

_NEM_ ::
  VFR_Waypoint
_NEM_ =
  VFR_Waypoint
    "NEMINGHA"
    (Just "NSW")
    "NEM"
    (Lat (-31) 7.5)
    (Lon 150 59.5)

_NPBR_ ::
  VFR_Waypoint
_NPBR_ =
  VFR_Waypoint
    "NEPEAN BRIDGE"
    (Just "NSW")
    "NPBR"
    (Lat (-33) 45.8)
    (Lon 150 39.6)

_NEN_ ::
  VFR_Waypoint
_NEN_ =
  VFR_Waypoint
    "NERANG"
    (Just "QLD")
    "NEN"
    (Lat (-27) 59.3)
    (Lon 153 20.3)

_NGI_ ::
  VFR_Waypoint
_NGI_ =
  VFR_Waypoint
    "NGULUPI"
    (Just "QLD")
    "NGI"
    (Lat (-10) 14.6)
    (Lon 142 24.6)

_NTT_ ::
  VFR_Waypoint
_NTT_ =
  VFR_Waypoint
    "NIMBIN TV TOWERS"
    (Just "NSW")
    "NTT"
    (Lat (-28) 32.5)
    (Lon 153 17.5)

_NIM_ ::
  VFR_Waypoint
_NIM_ =
  VFR_Waypoint
    "NIMROD PASSAGE"
    (Just "QLD")
    "NIM"
    (Lat (-12) 6.0)
    (Lon 143 47.0)

_NDI_ ::
  VFR_Waypoint
_NDI_ =
  VFR_Waypoint
    "NINDIGULLY"
    (Just "QLD")
    "NDI"
    (Lat (-28) 21.0)
    (Lon 148 49.0)

_NARL_ ::
  VFR_Waypoint
_NARL_ =
  VFR_Waypoint
    "NOARLUNGA"
    (Just "SA")
    "NARL"
    (Lat (-35) 8.5)
    (Lon 138 29.3)

_NBB_ ::
  VFR_Waypoint
_NBB_ =
  VFR_Waypoint
    "NOBBYS HEAD"
    (Just "NSW")
    "NBB"
    (Lat (-32) 54.9)
    (Lon 151 47.4)

_NDY_ ::
  VFR_Waypoint
_NDY_ =
  VFR_Waypoint
    "NODDY REEF"
    (Just "QLD")
    "NDY"
    (Lat (-13) 44.0)
    (Lon 143 45.0)

_NOME_ ::
  VFR_Waypoint
_NOME_ =
  VFR_Waypoint
    "NOME"
    (Just "QLD")
    "NOME"
    (Lat (-19) 22.6)
    (Lon 146 55.2)

_NNDO_ ::
  VFR_Waypoint
_NNDO_ =
  VFR_Waypoint
    "NOONDOO"
    (Just "QLD")
    "NNDO"
    (Lat (-28) 37.0)
    (Lon 148 26.0)

_NOSA_ ::
  VFR_Waypoint
_NOSA_ =
  VFR_Waypoint
    "NOOSA HEADS"
    (Just "QLD")
    "NOSA"
    (Lat (-26) 22.5)
    (Lon 153 7.0)

_NEQ_ ::
  VFR_Waypoint
_NEQ_ =
  VFR_Waypoint
    "NORTH EAST QUARRY"
    (Just "VIC")
    "NEQ"
    (Lat (-37) 56.5)
    (Lon 144 35.0)

_NOHD_ ::
  VFR_Waypoint
_NOHD_ =
  VFR_Waypoint
    "NORTH HEAD"
    (Just "NSW")
    "NOHD"
    (Lat (-33) 49.3)
    (Lon 151 17.5)

_NORT_ ::
  VFR_Waypoint
_NORT_ =
  VFR_Waypoint
    "NORTH LAKE"
    (Just "WA")
    "NORT"
    (Lat (-32) 4.6)
    (Lon 115 49.4)

_NSTA_ ::
  VFR_Waypoint
_NSTA_ =
  VFR_Waypoint
    "NORTH STAR"
    (Just "NSW")
    "NSTA"
    (Lat (-28) 55.0)
    (Lon 150 25.0)

_NBRR_ ::
  VFR_Waypoint
_NBRR_ =
  VFR_Waypoint
    "NORTHERN TIP BERSERKERS"
    (Just "QLD")
    "NBRR"
    (Lat (-23) 16.7)
    (Lon 150 35.3)

_NRWN_ ::
  VFR_Waypoint
_NRWN_ =
  VFR_Waypoint
    "NORWIN"
    (Just "QLD")
    "NRWN"
    (Lat (-27) 33.5)
    (Lon 151 22.8)

_NUDG_ ::
  VFR_Waypoint
_NUDG_ =
  VFR_Waypoint
    "NUDGEE TIP"
    (Just "QLD")
    "NUDG"
    (Lat (-27) 21.5)
    (Lon 153 5.5)

_NUA_ ::
  VFR_Waypoint
_NUA_ =
  VFR_Waypoint
    "NUNAMARA"
    (Just "TAS")
    "NUA"
    (Lat (-41) 23.5)
    (Lon 147 18.0)

_NUN_ ::
  VFR_Waypoint
_NUN_ =
  VFR_Waypoint
    "NUNDLE"
    (Just "NSW")
    "NUN"
    (Lat (-31) 28.0)
    (Lon 151 7.5)

_NUPA_ ::
  VFR_Waypoint
_NUPA_ =
  VFR_Waypoint
    "NURIOOTPA"
    (Just "SA")
    "NUPA"
    (Lat (-34) 29.0)
    (Lon 139 0.0)

_NCHU_ ::
  VFR_Waypoint
_NCHU_ =
  VFR_Waypoint
    "NYCHUM"
    (Just "QLD")
    "NCHU"
    (Lat (-16) 50.6)
    (Lon 144 27.7)

_OAT_ ::
  VFR_Waypoint
_OAT_ =
  VFR_Waypoint
    "OATLANDS"
    (Just "TAS")
    "OAT"
    (Lat (-42) 18.0)
    (Lon 147 22.0)

_OBC_ ::
  VFR_Waypoint
_OBC_ =
  VFR_Waypoint
    "OBSERVATION CITY"
    (Just "WA")
    "OBC"
    (Lat (-31) 53.7)
    (Lon 115 45.3)

_OBSH_ ::
  VFR_Waypoint
_OBSH_ =
  VFR_Waypoint
    "OBSERVATORY HILL"
    (Just "SA")
    "OBSH"
    (Lat (-28) 58.0)
    (Lon 132 0.0)

_OBY_ ::
  VFR_Waypoint
_OBY_ =
  VFR_Waypoint
    "OLD BOMBANDY"
    (Just "QLD")
    "OBY"
    (Lat (-22) 26.0)
    (Lon 148 38.0)

_OLCO_ ::
  VFR_Waypoint
_OLCO_ =
  VFR_Waypoint
    "OLD CORK"
    (Just "QLD")
    "OLCO"
    (Lat (-22) 56.0)
    (Lon 141 52.0)

_OLSOD_ ::
  VFR_Waypoint
_OLSOD_ =
  VFR_Waypoint
    "OLSOD"
    Nothing
    "OLSOD"
    (Lat (-8) 32.4)
    (Lon 144 27.2)

_ONPK_ ::
  VFR_Waypoint
_ONPK_ =
  VFR_Waypoint
    "ORAN PARK"
    (Just "NSW")
    "ONPK"
    (Lat (-34) 0.5)
    (Lon 150 44.5)

_ORKS_ ::
  VFR_Waypoint
_ORKS_ =
  VFR_Waypoint
    "ORCHARD ROCKS"
    (Just "QLD")
    "ORKS"
    (Lat (-19) 6.6)
    (Lon 146 52.9)

_ORF_ ::
  VFR_Waypoint
_ORF_ =
  VFR_Waypoint
    "ORFORD NESS"
    (Just "QLD")
    "ORF"
    (Lat (-11) 18.0)
    (Lon 142 49.0)

_OHB_ ::
  VFR_Waypoint
_OHB_ =
  VFR_Waypoint
    "OUTER HARBOR"
    (Just "SA")
    "OHB"
    (Lat (-34) 46.5)
    (Lon 138 29.0)

_OVL_ ::
  VFR_Waypoint
_OVL_ =
  VFR_Waypoint
    "OVERLANDER"
    (Just "WA")
    "OVL"
    (Lat (-26) 24.0)
    (Lon 114 28.0)

_OEN_ ::
  VFR_Waypoint
_OEN_ =
  VFR_Waypoint
    "OWEN"
    (Just "SA")
    "OEN"
    (Lat (-34) 16.2)
    (Lon 138 32.5)

_OWS_ ::
  VFR_Waypoint
_OWS_ =
  VFR_Waypoint
    "OWEN SPRINGS"
    (Just "NT")
    "OWS"
    (Lat (-23) 52.5)
    (Lon 133 28.3)

_OFD_ ::
  VFR_Waypoint
_OFD_ =
  VFR_Waypoint
    "OXENFORD"
    (Just "QLD")
    "OFD"
    (Lat (-27) 53.0)
    (Lon 153 19.0)

_PCVE_ ::
  VFR_Waypoint
_PCVE_ =
  VFR_Waypoint
    "PALM COVE"
    (Just "QLD")
    "PCVE"
    (Lat (-16) 45.0)
    (Lon 145 40.0)

_PFRM_ ::
  VFR_Waypoint
_PFRM_ =
  VFR_Waypoint
    "PALM FARM"
    (Just "QLD")
    "PFRM"
    (Lat (-17) 2.6)
    (Lon 145 45.7)

_PLW_ ::
  VFR_Waypoint
_PLW_ =
  VFR_Waypoint
    "PALM MEADOWS"
    (Just "QLD")
    "PLW"
    (Lat (-28) 2.0)
    (Lon 152 24.5)

_PLU_ ::
  VFR_Waypoint
_PLU_ =
  VFR_Waypoint
    "PALUMA DAM"
    (Just "QLD")
    "PLU"
    (Lat (-18) 57.3)
    (Lon 146 8.7)

_SFG_ ::
  VFR_Waypoint
_SFG_ =
  VFR_Waypoint
    "PARADISE GARDENS"
    (Just "QLD")
    "SFG"
    (Lat (-28) 1.1)
    (Lon 153 22.5)

_PKR_ ::
  VFR_Waypoint
_PKR_ =
  VFR_Waypoint
    "PARK RIDGE WATER TOWER"
    (Just "QLD")
    "PKR"
    (Lat (-27) 42.3)
    (Lon 153 2.3)

_PRKH_ ::
  VFR_Waypoint
_PRKH_ =
  VFR_Waypoint
    "PARKHURST"
    (Just "QLD")
    "PRKH"
    (Lat (-23) 18.2)
    (Lon 150 30.8)

_PRT_ ::
  VFR_Waypoint
_PRT_ =
  VFR_Waypoint
    "PARRAMATTA"
    (Just "NSW")
    "PRT"
    (Lat (-33) 49.0)
    (Lon 151 0.3)

_PAA_ ::
  VFR_Waypoint
_PAA_ =
  VFR_Waypoint
    "PATONGA"
    (Just "NSW")
    "PAA"
    (Lat (-33) 33.1)
    (Lon 151 15.8)

_PECO_ ::
  VFR_Waypoint
_PECO_ =
  VFR_Waypoint
    "PEACOCK"
    (Just "QLD")
    "PECO"
    (Lat (-18) 41.0)
    (Lon 145 59.0)

_PDNE_ ::
  VFR_Waypoint
_PDNE_ =
  VFR_Waypoint
    "PEAK DOWNS MINE"
    (Just "QLD")
    "PDNE"
    (Lat (-22) 15.0)
    (Lon 148 11.0)

_PEAR_ ::
  VFR_Waypoint
_PEAR_ =
  VFR_Waypoint
    "PEARSON ISLES"
    (Just "SA")
    "PEAR"
    (Lat (-33) 57.0)
    (Lon 134 16.0)

_PCCK_ ::
  VFR_Waypoint
_PCCK_ =
  VFR_Waypoint
    "PELICAN CREEK"
    (Just "QLD")
    "PCCK"
    (Lat (-25) 14.0)
    (Lon 150 54.0)

_PENH_ ::
  VFR_Waypoint
_PENH_ =
  VFR_Waypoint
    "PENNANT HILLS STROBE"
    (Just "NSW")
    "PENH"
    (Lat (-33) 44.4)
    (Lon 151 4.2)

_PENT_ ::
  VFR_Waypoint
_PENT_ =
  VFR_Waypoint
    "PENRITH"
    (Just "NSW")
    "PENT"
    (Lat (-33) 45.5)
    (Lon 150 42.0)

_PVS_ ::
  VFR_Waypoint
_PVS_ =
  VFR_Waypoint
    "PERCIVAL LAKES"
    (Just "WA")
    "PVS"
    (Lat (-21) 34.8)
    (Lon 124 9.6)

_PEG_ ::
  VFR_Waypoint
_PEG_ =
  VFR_Waypoint
    "PEREGIAN"
    (Just "QLD")
    "PEG"
    (Lat (-26) 31.0)
    (Lon 153 6.0)

_PCKD_ ::
  VFR_Waypoint
_PCKD_ =
  VFR_Waypoint
    "PERSEVERENCE CREEK DAM"
    (Just "QLD")
    "PCKD"
    (Lat (-27) 18.3)
    (Lon 152 7.3)

_PCTY_ ::
  VFR_Waypoint
_PCTY_ =
  VFR_Waypoint
    "PERTH CITY"
    (Just "WA")
    "PCTY"
    (Lat (-31) 57.3)
    (Lon 115 51.4)

_PTI_ ::
  VFR_Waypoint
_PTI_ =
  VFR_Waypoint
    "PETRIE"
    (Just "QLD")
    "PTI"
    (Lat (-27) 16.0)
    (Lon 152 58.5)

_PIB_ ::
  VFR_Waypoint
_PIB_ =
  VFR_Waypoint
    "PICKERING BROOK GOLF COURSE"
    (Just "WA")
    "PIB"
    (Lat (-32) 2.3)
    (Lon 116 6.7)

_PIL_ ::
  VFR_Waypoint
_PIL_ =
  VFR_Waypoint
    "PICKET HILL"
    (Just "NSW")
    "PIL"
    (Lat (-30) 34.3)
    (Lon 152 59.0)

_PCA_ ::
  VFR_Waypoint
_PCA_ =
  VFR_Waypoint
    "PICNIC BAY"
    (Just "QLD")
    "PCA"
    (Lat (-19) 11.0)
    (Lon 146 51.0)

_PNP_ ::
  VFR_Waypoint
_PNP_ =
  VFR_Waypoint
    "PICNIC PT"
    (Just "NSW")
    "PNP"
    (Lat (-33) 58.8)
    (Lon 151 0.1)

_PIC_ ::
  VFR_Waypoint
_PIC_ =
  VFR_Waypoint
    "PICTON"
    (Just "NSW")
    "PIC"
    (Lat (-34) 10.5)
    (Lon 150 37.0)

_PIG_ ::
  VFR_Waypoint
_PIG_ =
  VFR_Waypoint
    "PIGGERY"
    (Just "NSW")
    "PIG"
    (Lat (-36) 0.8)
    (Lon 146 47.5)

_PIPT_ ::
  VFR_Waypoint
_PIPT_ =
  VFR_Waypoint
    "PINE PT"
    (Just "SA")
    "PIPT"
    (Lat (-34) 34.0)
    (Lon 137 53.0)

_PING_ ::
  VFR_Waypoint
_PING_ =
  VFR_Waypoint
    "PINGARING"
    (Just "WA")
    "PING"
    (Lat (-32) 45.5)
    (Lon 118 37.5)

_PII_ ::
  VFR_Waypoint
_PII_ =
  VFR_Waypoint
    "PIRATE PT"
    (Just "QLD")
    "PII"
    (Lat (-23) 30.5)
    (Lon 150 38.5)

_PWH_ ::
  VFR_Waypoint
_PWH_ =
  VFR_Waypoint
    "PITTSWORTH"
    (Just "QLD")
    "PWH"
    (Lat (-27) 43.3)
    (Lon 151 38.0)

_PTOM_ ::
  VFR_Waypoint
_PTOM_ =
  VFR_Waypoint
    "POINT ORMOND"
    (Just "VIC")
    "PTOM"
    (Lat (-37) 53.0)
    (Lon 144 59.0)

_PSS_ ::
  VFR_Waypoint
_PSS_ =
  VFR_Waypoint
    "POINT STEPHENS LIGHTHOUSE"
    (Just "NSW")
    "PSS"
    (Lat (-32) 44.8)
    (Lon 152 12.5)

_PRP_ ::
  VFR_Waypoint
_PRP_ =
  VFR_Waypoint
    "PORPOISE PT"
    (Just "QLD")
    "PRP"
    (Lat (-27) 56.2)
    (Lon 153 25.5)

_PAL_ ::
  VFR_Waypoint
_PAL_ =
  VFR_Waypoint
    "PORT ADELAIDE"
    (Just "SA")
    "PAL"
    (Lat (-34) 51.0)
    (Lon 138 30.0)

_PMA_ ::
  VFR_Waypoint
_PMA_ =
  VFR_Waypoint
    "PORT ALMA"
    (Just "QLD")
    "PMA"
    (Lat (-23) 35.0)
    (Lon 150 51.5)

_POMP_ ::
  VFR_Waypoint
_POMP_ =
  VFR_Waypoint
    "PORT CAMPBELL"
    (Just "VIC")
    "POMP"
    (Lat (-38) 37.2)
    (Lon 142 59.7)

_PDV_ ::
  VFR_Waypoint
_PDV_ =
  VFR_Waypoint
    "PORT DAVEY"
    (Just "TAS")
    "PDV"
    (Lat (-43) 20.0)
    (Lon 145 53.0)

_PTD_ ::
  VFR_Waypoint
_PTD_ =
  VFR_Waypoint
    "PORT DOUGLAS"
    (Just "QLD")
    "PTD"
    (Lat (-16) 29.0)
    (Lon 145 27.8)

_PJUL_ ::
  VFR_Waypoint
_PJUL_ =
  VFR_Waypoint
    "PORT JULIA"
    (Just "SA")
    "PJUL"
    (Lat (-34) 40.0)
    (Lon 137 52.7)

_PMG_ ::
  VFR_Waypoint
_PMG_ =
  VFR_Waypoint
    "PORT MUSGRAVE"
    (Just "QLD")
    "PMG"
    (Lat (-12) 0.0)
    (Lon 141 56.0)

_PNE_ ::
  VFR_Waypoint
_PNE_ =
  VFR_Waypoint
    "PORT NEILL"
    (Just "SA")
    "PNE"
    (Lat (-34) 7.0)
    (Lon 136 21.0)

_PNL_ ::
  VFR_Waypoint
_PNL_ =
  VFR_Waypoint
    "PORT NOARLUNGA"
    (Just "SA")
    "PNL"
    (Lat (-35) 9.0)
    (Lon 138 28.0)

_PIPS_ ::
  VFR_Waypoint
_PIPS_ =
  VFR_Waypoint
    "PORT PHILLIP HEADS"
    (Just "VIC")
    "PIPS"
    (Lat (-38) 17.6)
    (Lon 144 38.0)

_PVCT_ ::
  VFR_Waypoint
_PVCT_ =
  VFR_Waypoint
    "PORT VINCENT"
    (Just "SA")
    "PVCT"
    (Lat (-34) 46.9)
    (Lon 137 51.7)

_POWR_ ::
  VFR_Waypoint
_POWR_ =
  VFR_Waypoint
    "POWERHOUSE"
    (Just "WA")
    "POWR"
    (Lat (-32) 5.7)
    (Lon 115 45.4)

_PWLC_ ::
  VFR_Waypoint
_PWLC_ =
  VFR_Waypoint
    "POWERLINE CROSSING"
    (Just "VIC")
    "PWLC"
    (Lat (-37) 36.8)
    (Lon 144 46.7)

_PRES_ ::
  VFR_Waypoint
_PRES_ =
  VFR_Waypoint
    "PRESCOTT LAKES"
    (Just "WA")
    "PRES"
    (Lat (-20) 45.0)
    (Lon 125 10.0)

_PRS_ ::
  VFR_Waypoint
_PRS_ =
  VFR_Waypoint
    "PRIMROSE SANDS"
    (Just "TAS")
    "PRS"
    (Lat (-42) 53.0)
    (Lon 147 40.0)

_PCB_ ::
  VFR_Waypoint
_PCB_ =
  VFR_Waypoint
    "PRINCESS CHARLOTTE BAY"
    (Just "QLD")
    "PCB"
    (Lat (-14) 20.0)
    (Lon 144 7.0)

_PSP_ ::
  VFR_Waypoint
_PSP_ =
  VFR_Waypoint
    "PROSPECT RESV"
    (Just "NSW")
    "PSP"
    (Lat (-33) 49.0)
    (Lon 150 55.0)

_PSF_ ::
  VFR_Waypoint
_PSF_ =
  VFR_Waypoint
    "PROSSERS SUGARLOAF"
    (Just "TAS")
    "PSF"
    (Lat (-42) 40.5)
    (Lon 147 49.5)

_PSTO_ ::
  VFR_Waypoint
_PSTO_ =
  VFR_Waypoint
    "PROSTON"
    (Just "QLD")
    "PSTO"
    (Lat (-26) 10.0)
    (Lon 151 36.0)

_DNGR_ ::
  VFR_Waypoint
_DNGR_ =
  VFR_Waypoint
    "PT DANGER"
    (Just "NSW")
    "DNGR"
    (Lat (-28) 9.9)
    (Lon 153 33.1)

_FAW_ ::
  VFR_Waypoint
_FAW_ =
  VFR_Waypoint
    "PT FAWCETT"
    (Just "NT")
    "FAW"
    (Lat (-11) 48.0)
    (Lon 130 1.0)

_PMPH_ ::
  VFR_Waypoint
_PMPH_ =
  VFR_Waypoint
    "PUMPHREY'S BRIDGE"
    (Just "WA")
    "PMPH"
    (Lat (-32) 40.0)
    (Lon 116 54.0)

_PBF_ ::
  VFR_Waypoint
_PBF_ =
  VFR_Waypoint
    "PURLINGBROOKE FALLS"
    (Just "QLD")
    "PBF"
    (Lat (-28) 10.0)
    (Lon 153 16.0)

_PWDA_ ::
  VFR_Waypoint
_PWDA_ =
  VFR_Waypoint
    "PURRAWUNDA"
    (Just "QLD")
    "PWDA"
    (Lat (-27) 32.3)
    (Lon 151 37.5)

_PUTY_ ::
  VFR_Waypoint
_PUTY_ =
  VFR_Waypoint
    "PUTTY"
    (Just "NSW")
    "PUTY"
    (Lat (-32) 58.0)
    (Lon 150 45.0)

_PYA_ ::
  VFR_Waypoint
_PYA_ =
  VFR_Waypoint
    "PYALONG"
    (Just "VIC")
    "PYA"
    (Lat (-37) 7.0)
    (Lon 144 51.3)

_PYK_ ::
  VFR_Waypoint
_PYK_ =
  VFR_Waypoint
    "PYKES CREEK RESV"
    (Just "VIC")
    "PYK"
    (Lat (-37) 36.0)
    (Lon 144 17.7)

_QE2_ ::
  VFR_Waypoint
_QE2_ =
  VFR_Waypoint
    "QE2 STADIUM"
    (Just "QLD")
    "QE2"
    (Lat (-27) 33.5)
    (Lon 153 3.8)

_QAI_ ::
  VFR_Waypoint
_QAI_ =
  VFR_Waypoint
    "QUAIL ISLAND"
    (Just "QLD")
    "QAI"
    (Lat (-22) 8.0)
    (Lon 150 0.0)

_QBN_ ::
  VFR_Waypoint
_QBN_ =
  VFR_Waypoint
    "QUEANBEYAN"
    (Just "NSW")
    "QBN"
    (Lat (-35) 21.6)
    (Lon 149 14.6)

_Q1_ ::
  VFR_Waypoint
_Q1_ =
  VFR_Waypoint
    "QUEBEC ONE"
    (Just "QLD")
    "Q1"
    (Lat (-28) 0.4)
    (Lon 153 25.8)

_QLW_ ::
  VFR_Waypoint
_QLW_ =
  VFR_Waypoint
    "QUINALOW"
    (Just "QLD")
    "QLW"
    (Lat (-27) 6.3)
    (Lon 151 37.3)

_QNDI_ ::
  VFR_Waypoint
_QNDI_ =
  VFR_Waypoint
    "QUIRINDI"
    (Just "NSW")
    "QNDI"
    (Lat (-31) 29.4)
    (Lon 150 30.8)

_RIL_ ::
  VFR_Waypoint
_RIL_ =
  VFR_Waypoint
    "RABBIT ISLAND"
    (Just "QLD")
    "RIL"
    (Lat (-20) 50.0)
    (Lon 148 54.0)

_RBY_ ::
  VFR_Waypoint
_RBY_ =
  VFR_Waypoint
    "RABY BAY"
    (Just "QLD")
    "RBY"
    (Lat (-27) 31.0)
    (Lon 153 16.5)

_RDRS_ ::
  VFR_Waypoint
_RDRS_ =
  VFR_Waypoint
    "RADAR SITE"
    (Just "QLD")
    "RDRS"
    (Lat (-19) 12.0)
    (Lon 146 46.0)

_RADT_ ::
  VFR_Waypoint
_RADT_ =
  VFR_Waypoint
    "RADIO TELESCOPE"
    (Just "TAS")
    "RADT"
    (Lat (-42) 49.0)
    (Lon 147 27.0)

_RAIS_ ::
  VFR_Waypoint
_RAIS_ =
  VFR_Waypoint
    "RAINE ISLAND"
    (Just "QLD")
    "RAIS"
    (Lat (-11) 36.0)
    (Lon 144 2.0)

_RNN_ ::
  VFR_Waypoint
_RNN_ =
  VFR_Waypoint
    "RANNES"
    (Just "QLD")
    "RNN"
    (Lat (-24) 6.0)
    (Lon 150 7.0)

_RAPD_ ::
  VFR_Waypoint
_RAPD_ =
  VFR_Waypoint
    "RAPID BAY"
    (Just "SA")
    "RAPD"
    (Lat (-35) 31.3)
    (Lon 138 11.0)

_RTY_ ::
  VFR_Waypoint
_RTY_ =
  VFR_Waypoint
    "RATHDOWNEY"
    (Just "QLD")
    "RTY"
    (Lat (-28) 13.0)
    (Lon 152 52.0)

_RKI_ ::
  VFR_Waypoint
_RKI_ =
  VFR_Waypoint
    "RATTLESNAKE ISLAND"
    (Just "QLD")
    "RKI"
    (Lat (-19) 2.0)
    (Lon 146 36.7)

_RCS_ ::
  VFR_Waypoint
_RCS_ =
  VFR_Waypoint
    "RED CLIFFS"
    (Just "VIC")
    "RCS"
    (Lat (-34) 18.0)
    (Lon 142 13.0)

_RDHI_ ::
  VFR_Waypoint
_RDHI_ =
  VFR_Waypoint
    "RED HILL"
    (Just "QLD")
    "RDHI"
    (Lat (-21) 38.0)
    (Lon 148 3.0)

_RER_ ::
  VFR_Waypoint
_RER_ =
  VFR_Waypoint
    "RED ROCK"
    (Just "NSW")
    "RER"
    (Lat (-29) 59.2)
    (Lon 153 13.5)

_REDC_ ::
  VFR_Waypoint
_REDC_ =
  VFR_Waypoint
    "REDCLIFFE BRIDGE"
    (Just "WA")
    "REDC"
    (Lat (-31) 55.8)
    (Lon 115 56.3)

_RDV_ ::
  VFR_Waypoint
_RDV_ =
  VFR_Waypoint
    "REDCLIFFE VALE HS"
    (Just "QLD")
    "RDV"
    (Lat (-21) 7.0)
    (Lon 148 7.0)

_REDF_ ::
  VFR_Waypoint
_REDF_ =
  VFR_Waypoint
    "REDFERN RAILWAY STATION"
    (Just "NSW")
    "REDF"
    (Lat (-33) 53.5)
    (Lon 151 11.9)

_REDB_ ::
  VFR_Waypoint
_REDB_ =
  VFR_Waypoint
    "REDLAND BAY"
    (Just "QLD")
    "REDB"
    (Lat (-27) 36.0)
    (Lon 153 18.0)

_REDL_ ::
  VFR_Waypoint
_REDL_ =
  VFR_Waypoint
    "REDLYNCH"
    (Just "QLD")
    "REDL"
    (Lat (-16) 53.0)
    (Lon 145 42.0)

_RENR_ ::
  VFR_Waypoint
_RENR_ =
  VFR_Waypoint
    "RENNER SPRINGS"
    (Just "NT")
    "RENR"
    (Lat (-18) 19.0)
    (Lon 133 48.0)

_RESC_ ::
  VFR_Waypoint
_RESC_ =
  VFR_Waypoint
    "RESEARCH CENTRE"
    (Just "NT")
    "RESC"
    (Lat (-14) 46.8)
    (Lon 131 56.2)

_RCH_ ::
  VFR_Waypoint
_RCH_ =
  VFR_Waypoint
    "RICHMOND"
    (Just "TAS")
    "RCH"
    (Lat (-42) 44.0)
    (Lon 147 26.0)

_RIT_ ::
  VFR_Waypoint
_RIT_ =
  VFR_Waypoint
    "RING TANK"
    (Just "QLD")
    "RIT"
    (Lat (-26) 45.0)
    (Lon 153 5.5)

_RMH_ ::
  VFR_Waypoint
_RMH_ =
  VFR_Waypoint
    "RIVER MOUTH"
    (Just "SA")
    "RMH"
    (Lat (-34) 35.0)
    (Lon 138 21.5)

_RIV_ ::
  VFR_Waypoint
_RIV_ =
  VFR_Waypoint
    "RIVERINA CAMPUS"
    (Just "NSW")
    "RIV"
    (Lat (-35) 3.3)
    (Lon 147 20.8)

_RVTN_ ::
  VFR_Waypoint
_RVTN_ =
  VFR_Waypoint
    "RIVERTON"
    (Just "SA")
    "RVTN"
    (Lat (-34) 9.9)
    (Lon 138 44.8)

_ROTC_ ::
  VFR_Waypoint
_ROTC_ =
  VFR_Waypoint
    "ROBINA TOWN CENTRE"
    (Just "QLD")
    "ROTC"
    (Lat (-28) 4.6)
    (Lon 153 23.1)

_ROK_ ::
  VFR_Waypoint
_ROK_ =
  VFR_Waypoint
    "ROCKBANK"
    (Just "VIC")
    "ROK"
    (Lat (-37) 43.7)
    (Lon 144 39.2)

_ROHM_ ::
  VFR_Waypoint
_ROHM_ =
  VFR_Waypoint
    "ROCKINGHAM"
    (Just "WA")
    "ROHM"
    (Lat (-32) 17.5)
    (Lon 115 44.5)

_RLY_ ::
  VFR_Waypoint
_RLY_ =
  VFR_Waypoint
    "ROLEYSTONE"
    (Just "WA")
    "RLY"
    (Lat (-32) 7.0)
    (Lon 116 4.5)

_RGS_ ::
  VFR_Waypoint
_RGS_ =
  VFR_Waypoint
    "ROLLINGSTONE"
    (Just "QLD")
    "RGS"
    (Lat (-19) 2.8)
    (Lon 146 23.3)

_RKWC_ ::
  VFR_Waypoint
_RKWC_ =
  VFR_Waypoint
    "ROOKWOOD CEMETERY"
    (Just "NSW")
    "RKWC"
    (Lat (-33) 52.5)
    (Lon 151 3.3)

_RSH_ ::
  VFR_Waypoint
_RSH_ =
  VFR_Waypoint
    "ROSEHILL RACECOURSE"
    (Just "NSW")
    "RSH"
    (Lat (-33) 49.5)
    (Lon 151 1.5)

_RVB_ ::
  VFR_Waypoint
_RVB_ =
  VFR_Waypoint
    "ROSEVILLE BRIDGE"
    (Just "NSW")
    "RVB"
    (Lat (-33) 46.0)
    (Lon 151 12.0)

_RSWD_ ::
  VFR_Waypoint
_RSWD_ =
  VFR_Waypoint
    "ROSEWOOD"
    (Just "QLD")
    "RSWD"
    (Lat (-27) 38.2)
    (Lon 152 35.6)

_RSEW_ ::
  VFR_Waypoint
_RSEW_ =
  VFR_Waypoint
    "ROSEWOOD ISLAND"
    (Just "QLD")
    "RSEW"
    (Lat (-22) 24.0)
    (Lon 149 44.0)

_RRDM_ ::
  VFR_Waypoint
_RRDM_ =
  VFR_Waypoint
    "ROSS RIVER DAM"
    (Just "QLD")
    "RRDM"
    (Lat (-19) 24.7)
    (Lon 146 44.0)

_RLR_ ::
  VFR_Waypoint
_RLR_ =
  VFR_Waypoint
    "ROSSLYNNE RESV"
    (Just "VIC")
    "RLR"
    (Lat (-37) 28.2)
    (Lon 144 33.8)

_RMT_ ::
  VFR_Waypoint
_RMT_ =
  VFR_Waypoint
    "ROUND MT"
    (Just "QLD")
    "RMT"
    (Lat (-19) 27.7)
    (Lon 146 41.7)

_RCB_ ::
  VFR_Waypoint
_RCB_ =
  VFR_Waypoint
    "RUSH CUTTERS BAY"
    (Just "NSW")
    "RCB"
    (Lat (-33) 52.4)
    (Lon 151 13.9)

_RUIS_ ::
  VFR_Waypoint
_RUIS_ =
  VFR_Waypoint
    "RUSSELL ISLAND"
    (Just "QLD")
    "RUIS"
    (Lat (-27) 40.0)
    (Lon 153 23.0)

_RYB_ ::
  VFR_Waypoint
_RYB_ =
  VFR_Waypoint
    "RYDE BRIDGE"
    (Just "NSW")
    "RYB"
    (Lat (-33) 49.5)
    (Lon 151 5.5)

_SADD_ ::
  VFR_Waypoint
_SADD_ =
  VFR_Waypoint
    "SADDLE MT"
    (Just "QLD")
    "SADD"
    (Lat (-16) 49.2)
    (Lon 145 39.0)

_SSV_ ::
  VFR_Waypoint
_SSV_ =
  VFR_Waypoint
    "SAMSONVALE"
    (Just "QLD")
    "SSV"
    (Lat (-27) 16.7)
    (Lon 152 51.3)

_SAU_ ::
  VFR_Waypoint
_SAU_ =
  VFR_Waypoint
    "SANCTUARY COVE"
    (Just "QLD")
    "SAU"
    (Lat (-27) 51.5)
    (Lon 153 22.5)

_SALW_ ::
  VFR_Waypoint
_SALW_ =
  VFR_Waypoint
    "SANDALWOOD"
    (Just "SA")
    "SALW"
    (Lat (-34) 57.0)
    (Lon 140 8.0)

_SSTO_ ::
  VFR_Waypoint
_SSTO_ =
  VFR_Waypoint
    "SANDERSTON"
    (Just "SA")
    "SSTO"
    (Lat (-34) 44.6)
    (Lon 139 15.1)

_SAND_ ::
  VFR_Waypoint
_SAND_ =
  VFR_Waypoint
    "SANDGATE PIER"
    (Just "QLD")
    "SAND"
    (Lat (-27) 19.7)
    (Lon 153 5.3)

_SDP_ ::
  VFR_Waypoint
_SDP_ =
  VFR_Waypoint
    "SANDY PT"
    (Just "VIC")
    "SDP"
    (Lat (-38) 25.0)
    (Lon 145 14.0)

_STT_ ::
  VFR_Waypoint
_STT_ =
  VFR_Waypoint
    "SANTA TERESA"
    (Just "NT")
    "STT"
    (Lat (-24) 8.0)
    (Lon 134 22.4)

_SJI_ ::
  VFR_Waypoint
_SJI_ =
  VFR_Waypoint
    "SARAJI"
    (Just "QLD")
    "SJI"
    (Lat (-22) 26.0)
    (Lon 148 17.0)

_SRIN_ ::
  VFR_Waypoint
_SRIN_ =
  VFR_Waypoint
    "SARINA"
    (Just "QLD")
    "SRIN"
    (Lat (-21) 25.5)
    (Lon 149 13.0)

_SVR_ ::
  VFR_Waypoint
_SVR_ =
  VFR_Waypoint
    "SAVAGE RIVER"
    (Just "TAS")
    "SVR"
    (Lat (-41) 35.0)
    (Lon 145 8.0)

_SWTE_ ::
  VFR_Waypoint
_SWTE_ =
  VFR_Waypoint
    "SAWTELL"
    (Just "NSW")
    "SWTE"
    (Lat (-30) 22.0)
    (Lon 153 6.0)

_SWY_ ::
  VFR_Waypoint
_SWY_ =
  VFR_Waypoint
    "SAWYERS VALLEY"
    (Just "WA")
    "SWY"
    (Lat (-31) 54.3)
    (Lon 116 12.3)

_STC_ ::
  VFR_Waypoint
_STC_ =
  VFR_Waypoint
    "SCOTT CREEK"
    (Just "NT")
    "STC"
    (Lat (-14) 50.0)
    (Lon 131 50.0)

_STTE_ ::
  VFR_Waypoint
_STTE_ =
  VFR_Waypoint
    "SCOTTSDALE"
    (Just "TAS")
    "STTE"
    (Lat (-41) 10.0)
    (Lon 147 31.0)

_SECF_ ::
  VFR_Waypoint
_SECF_ =
  VFR_Waypoint
    "SEA CLIFF BRIDGE"
    (Just "NSW")
    "SECF"
    (Lat (-34) 15.2)
    (Lon 150 58.5)

_SVAL_ ::
  VFR_Waypoint
_SVAL_ =
  VFR_Waypoint
    "SECOND VALLEY"
    (Just "SA")
    "SVAL"
    (Lat (-35) 31.0)
    (Lon 138 13.0)

_SLB_ ::
  VFR_Waypoint
_SLB_ =
  VFR_Waypoint
    "SELLICKS BEACH"
    (Just "SA")
    "SLB"
    (Lat (-35) 20.5)
    (Lon 138 27.0)

_SDS_ ::
  VFR_Waypoint
_SDS_ =
  VFR_Waypoint
    "SHAUNA DOWNS"
    (Just "QLD")
    "SDS"
    (Lat (-24) 37.0)
    (Lon 149 55.0)

_SHCR_ ::
  VFR_Waypoint
_SHCR_ =
  VFR_Waypoint
    "SHAW CREEK"
    (Just "NT")
    "SHCR"
    (Lat (-25) 13.0)
    (Lon 129 43.8)

_SHI_ ::
  VFR_Waypoint
_SHI_ =
  VFR_Waypoint
    "SHAW ISLAND"
    (Just "QLD")
    "SHI"
    (Lat (-20) 30.8)
    (Lon 149 5.2)

_SHL_ ::
  VFR_Waypoint
_SHL_ =
  VFR_Waypoint
    "SHELBURNE BAY"
    (Just "QLD")
    "SHL"
    (Lat (-11) 53.0)
    (Lon 143 1.0)

_SEL_ ::
  VFR_Waypoint
_SEL_ =
  VFR_Waypoint
    "SHELLEY BRIDGE"
    (Just "WA")
    "SEL"
    (Lat (-32) 1.5)
    (Lon 115 54.0)

_SHOAL_ ::
  VFR_Waypoint
_SHOAL_ =
  VFR_Waypoint
    "SHOAL"
    (Just "VIC")
    "SHOAL"
    (Lat (-38) 4.0)
    (Lon 145 2.0)

_SSG_ ::
  VFR_Waypoint
_SSG_ =
  VFR_Waypoint
    "SIMPSONS GAP"
    (Just "NT")
    "SSG"
    (Lat (-23) 40.8)
    (Lon 133 43.0)

_SIXS_ ::
  VFR_Waypoint
_SIXS_ =
  VFR_Waypoint
    "SIX SOUTH"
    (Just "WA")
    "SIXS"
    (Lat (-32) 11.1)
    (Lon 115 56.0)

_SKP_ ::
  VFR_Waypoint
_SKP_ =
  VFR_Waypoint
    "SKIPTON"
    (Just "VIC")
    "SKP"
    (Lat (-37) 41.0)
    (Lon 143 22.0)

_SLPT_ ::
  VFR_Waypoint
_SLPT_ =
  VFR_Waypoint
    "SLADE POINT"
    (Just "QLD")
    "SLPT"
    (Lat (-21) 3.9)
    (Lon 149 13.5)

_SGK_ ::
  VFR_Waypoint
_SGK_ =
  VFR_Waypoint
    "SLOPING HUMMOCK"
    (Just "QLD")
    "SGK"
    (Lat (-24) 51.0)
    (Lon 152 26.0)

_SLP_ ::
  VFR_Waypoint
_SLP_ =
  VFR_Waypoint
    "SLOPING ISLAND"
    (Just "TAS")
    "SLP"
    (Lat (-42) 56.9)
    (Lon 147 38.9)

_SMIF_ ::
  VFR_Waypoint
_SMIF_ =
  VFR_Waypoint
    "SMITHFIELD"
    (Just "QLD")
    "SMIF"
    (Lat (-16) 50.0)
    (Lon 145 41.0)

_SYI_ ::
  VFR_Waypoint
_SYI_ =
  VFR_Waypoint
    "SNOWY INTERSECTION"
    (Just "NSW")
    "SYI"
    (Lat (-35) 11.0)
    (Lon 147 52.1)

_SOFA_ ::
  VFR_Waypoint
_SOFA_ =
  VFR_Waypoint
    "SOFALA"
    (Just "NSW")
    "SOFA"
    (Lat (-33) 1.0)
    (Lon 149 41.0)

_SRP_ ::
  VFR_Waypoint
_SRP_ =
  VFR_Waypoint
    "SOLDIERS POINT"
    (Just "NSW")
    "SRP"
    (Lat (-32) 42.0)
    (Lon 152 3.8)

_SMD_ ::
  VFR_Waypoint
_SMD_ =
  VFR_Waypoint
    "SOMERSET DAM"
    (Just "QLD")
    "SMD"
    (Lat (-27) 7.3)
    (Lon 152 33.0)

_SMN_ ::
  VFR_Waypoint
_SMN_ =
  VFR_Waypoint
    "SOMERTON"
    (Just "NSW")
    "SMN"
    (Lat (-30) 56.5)
    (Lon 150 38.2)

_SORL_ ::
  VFR_Waypoint
_SORL_ =
  VFR_Waypoint
    "SORELL"
    (Just "TAS")
    "SORL"
    (Lat (-42) 47.0)
    (Lon 147 35.0)

_SEC_ ::
  VFR_Waypoint
_SEC_ =
  VFR_Waypoint
    "SOUTH EAST CAPE"
    (Just "TAS")
    "SEC"
    (Lat (-43) 39.0)
    (Lon 146 49.0)

_SMIB_ ::
  VFR_Waypoint
_SMIB_ =
  VFR_Waypoint
    "SOUTH MISSION BEACH"
    (Just "QLD")
    "SMIB"
    (Lat (-17) 56.9)
    (Lon 146 5.5)

_SPR_ ::
  VFR_Waypoint
_SPR_ =
  VFR_Waypoint
    "SOUTH PARA RESV"
    (Just "SA")
    "SPR"
    (Lat (-34) 41.0)
    (Lon 138 52.0)

_SPN_ ::
  VFR_Waypoint
_SPN_ =
  VFR_Waypoint
    "SOUTH PINNACLE"
    (Just "QLD")
    "SPN"
    (Lat (-19) 24.5)
    (Lon 146 38.0)

_SDG_ ::
  VFR_Waypoint
_SDG_ =
  VFR_Waypoint
    "SOUTHEDGE"
    (Just "QLD")
    "SDG"
    (Lat (-16) 49.0)
    (Lon 145 13.0)

_SBRR_ ::
  VFR_Waypoint
_SBRR_ =
  VFR_Waypoint
    "SOUTHERN TIP BERSERKERS"
    (Just "QLD")
    "SBRR"
    (Lat (-23) 24.0)
    (Lon 150 37.5)

_SPT_ ::
  VFR_Waypoint
_SPT_ =
  VFR_Waypoint
    "SOUTHPORT"
    (Just "QLD")
    "SPT"
    (Lat (-27) 55.0)
    (Lon 153 22.3)

_STR_ ::
  VFR_Waypoint
_STR_ =
  VFR_Waypoint
    "SOUTHPORT ROAD"
    (Just "NT")
    "STR"
    (Lat (-12) 39.0)
    (Lon 130 46.5)

_SPIT_ ::
  VFR_Waypoint
_SPIT_ =
  VFR_Waypoint
    "SPIT BRIDGE"
    (Just "NSW")
    "SPIT"
    (Lat (-33) 48.2)
    (Lon 151 14.8)

_SRR_ ::
  VFR_Waypoint
_SRR_ =
  VFR_Waypoint
    "SPLIT ROCK RESV"
    (Just "NSW")
    "SRR"
    (Lat (-30) 34.5)
    (Lon 150 42.0)

_SOI_ ::
  VFR_Waypoint
_SOI_ =
  VFR_Waypoint
    "SPLIT SOLITARY ISLAND"
    (Just "NSW")
    "SOI"
    (Lat (-30) 14.5)
    (Lon 153 10.8)

_SPMT_ ::
  VFR_Waypoint
_SPMT_ =
  VFR_Waypoint
    "SPRING MOUNTAIN"
    (Just "QLD")
    "SPMT"
    (Lat (-27) 42.8)
    (Lon 152 53.1)

_SBK_ ::
  VFR_Waypoint
_SBK_ =
  VFR_Waypoint
    "SPRINGBROOK"
    (Just "NSW")
    "SBK"
    (Lat (-28) 13.9)
    (Lon 153 16.7)

_SGM_ ::
  VFR_Waypoint
_SGM_ =
  VFR_Waypoint
    "ST GEORGES MINE"
    (Just "QLD")
    "SGM"
    (Lat (-16) 30.0)
    (Lon 144 24.0)

_SHIS_ ::
  VFR_Waypoint
_SHIS_ =
  VFR_Waypoint
    "ST HELENA ISLAND"
    (Just "QLD")
    "SHIS"
    (Lat (-27) 22.7)
    (Lon 153 14.0)

_SIS_ ::
  VFR_Waypoint
_SIS_ =
  VFR_Waypoint
    "ST IVES SHOWGROUND"
    (Just "NSW")
    "SIS"
    (Lat (-33) 42.3)
    (Lon 151 11.0)

_SKI_ ::
  VFR_Waypoint
_SKI_ =
  VFR_Waypoint
    "ST KILDA"
    (Just "SA")
    "SKI"
    (Lat (-34) 44.5)
    (Lon 138 31.8)

_SRY_ ::
  VFR_Waypoint
_SRY_ =
  VFR_Waypoint
    "STANSBURY"
    (Just "SA")
    "SRY"
    (Lat (-34) 54.7)
    (Lon 137 47.3)

_SLL_ ::
  VFR_Waypoint
_SLL_ =
  VFR_Waypoint
    "STANWELL PARK"
    (Just "NSW")
    "SLL"
    (Lat (-34) 13.7)
    (Lon 150 59.3)

_SPS_ ::
  VFR_Waypoint
_SPS_ =
  VFR_Waypoint
    "STANWELL POWER STN"
    (Just "QLD")
    "SPS"
    (Lat (-23) 30.0)
    (Lon 150 20.0)

_STARF_ ::
  VFR_Waypoint
_STARF_ =
  VFR_Waypoint
    "STARF"
    Nothing
    "STARF"
    (Lat (-9) 7.6)
    (Lon 146 43.5)

_SNP_ ::
  VFR_Waypoint
_SNP_ =
  VFR_Waypoint
    "STATION PIER"
    (Just "VIC")
    "SNP"
    (Lat (-37) 50.9)
    (Lon 144 55.8)

_STPK_ ::
  VFR_Waypoint
_STPK_ =
  VFR_Waypoint
    "STEPHENSONS PEAK"
    (Just "NT")
    "STPK"
    (Lat (-25) 30.0)
    (Lon 130 11.0)

_SCRK_ ::
  VFR_Waypoint
_SCRK_ =
  VFR_Waypoint
    "STONEY CREEK"
    (Just "QLD")
    "SCRK"
    (Lat (-16) 52.7)
    (Lon 145 39.3)

_SBD_ ::
  VFR_Waypoint
_SBD_ =
  VFR_Waypoint
    "STORY BRIDGE"
    (Just "QLD")
    "SBD"
    (Lat (-27) 27.9)
    (Lon 153 2.3)

_STOT_ ::
  VFR_Waypoint
_STOT_ =
  VFR_Waypoint
    "STOTTS ISLAND"
    (Just "NSW")
    "STOT"
    (Lat (-28) 16.1)
    (Lon 153 30.0)

_SYN_ ::
  VFR_Waypoint
_SYN_ =
  VFR_Waypoint
    "STRATHALBYN"
    (Just "SA")
    "SYN"
    (Lat (-35) 15.5)
    (Lon 138 53.7)

_SFE_ ::
  VFR_Waypoint
_SFE_ =
  VFR_Waypoint
    "STRATHFINELLA"
    (Just "QLD")
    "SFE"
    (Lat (-23) 21.0)
    (Lon 143 33.0)

_STRA_ ::
  VFR_Waypoint
_STRA_ =
  VFR_Waypoint
    "STRATHGORDON"
    (Just "TAS")
    "STRA"
    (Lat (-42) 46.0)
    (Lon 146 2.2)

_SLY_ ::
  VFR_Waypoint
_SLY_ =
  VFR_Waypoint
    "STRELLEY HS"
    (Just "WA")
    "SLY"
    (Lat (-20) 26.5)
    (Lon 118 59.0)

_SRO_ ::
  VFR_Waypoint
_SRO_ =
  VFR_Waypoint
    "STROUD ROAD"
    (Just "NSW")
    "SRO"
    (Lat (-32) 20.8)
    (Lon 151 55.1)

_SUA_ ::
  VFR_Waypoint
_SUA_ =
  VFR_Waypoint
    "STUART"
    (Just "QLD")
    "SUA"
    (Lat (-19) 21.0)
    (Lon 146 50.0)

_STUM_ ::
  VFR_Waypoint
_STUM_ =
  VFR_Waypoint
    "STUMERS CREEK"
    (Just "QLD")
    "STUM"
    (Lat (-26) 31.2)
    (Lon 153 5.1)

_SUI_ ::
  VFR_Waypoint
_SUI_ =
  VFR_Waypoint
    "STURT INTERSECTION"
    (Just "NSW")
    "SUI"
    (Lat (-35) 13.2)
    (Lon 147 47.6)

_SUB_ ::
  VFR_Waypoint
_SUB_ =
  VFR_Waypoint
    "SUBSTATION"
    (Just "SA")
    "SUB"
    (Lat (-34) 44.2)
    (Lon 138 42.8)

_SUTR_ ::
  VFR_Waypoint
_SUTR_ =
  VFR_Waypoint
    "SUGAR TERMINAL"
    (Just "QLD")
    "SUTR"
    (Lat (-16) 56.5)
    (Lon 145 46.0)

_SLMT_ ::
  VFR_Waypoint
_SLMT_ =
  VFR_Waypoint
    "SUGARLOAF MT"
    (Just "NSW")
    "SLMT"
    (Lat (-31) 26.0)
    (Lon 150 52.5)

_SUG_ ::
  VFR_Waypoint
_SUG_ =
  VFR_Waypoint
    "SUGARLOAF PT"
    (Just "NSW")
    "SUG"
    (Lat (-32) 26.7)
    (Lon 152 32.4)

_SGSV_ ::
  VFR_Waypoint
_SGSV_ =
  VFR_Waypoint
    "SUGARLOAF RESERVOIR"
    (Just "VIC")
    "SGSV"
    (Lat (-37) 40.5)
    (Lon 145 18.0)

_SWLD_ ::
  VFR_Waypoint
_SWLD_ =
  VFR_Waypoint
    "SUGARWORLD"
    (Just "QLD")
    "SWLD"
    (Lat (-17) 1.0)
    (Lon 145 43.9)

_SUNZ_ ::
  VFR_Waypoint
_SUNZ_ =
  VFR_Waypoint
    "SUN ZINC REFINERY"
    (Just "QLD")
    "SUNZ"
    (Lat (-19) 20.0)
    (Lon 146 53.2)

_SBU_ ::
  VFR_Waypoint
_SBU_ =
  VFR_Waypoint
    "SUNBURY"
    (Just "VIC")
    "SBU"
    (Lat (-37) 35.0)
    (Lon 144 43.5)

_SWT_ ::
  VFR_Waypoint
_SWT_ =
  VFR_Waypoint
    "SUNBURY WATER TANK"
    (Just "VIC")
    "SWT"
    (Lat (-37) 32.9)
    (Lon 144 41.5)

_SBIT_ ::
  VFR_Waypoint
_SBIT_ =
  VFR_Waypoint
    "SURBITON"
    (Just "QLD")
    "SBIT"
    (Lat (-23) 9.0)
    (Lon 146 37.0)

_SUPA_ ::
  VFR_Waypoint
_SUPA_ =
  VFR_Waypoint
    "SURFER'S PARADISE"
    (Just "QLD")
    "SUPA"
    (Lat (-28) 0.0)
    (Lon 153 26.0)

_SUD_ ::
  VFR_Waypoint
_SUD_ =
  VFR_Waypoint
    "SUTHERLAND"
    (Just "NSW")
    "SUD"
    (Lat (-34) 2.3)
    (Lon 151 3.3)

_SHER_ ::
  VFR_Waypoint
_SHER_ =
  VFR_Waypoint
    "SUTHERLANDS"
    (Just "SA")
    "SHER"
    (Lat (-34) 9.0)
    (Lon 139 14.0)

_SUE_ ::
  VFR_Waypoint
_SUE_ =
  VFR_Waypoint
    "SUTTON ROAD OVERPASS"
    (Just "NSW")
    "SUE"
    (Lat (-35) 11.0)
    (Lon 149 15.5)

_SBCH_ ::
  VFR_Waypoint
_SBCH_ =
  VFR_Waypoint
    "SUTTONS BEACH"
    (Just "QLD")
    "SBCH"
    (Lat (-27) 14.1)
    (Lon 153 6.9)

_SCTY_ ::
  VFR_Waypoint
_SCTY_ =
  VFR_Waypoint
    "SYDNEY CBD"
    (Just "NSW")
    "SCTY"
    (Lat (-33) 52.0)
    (Lon 151 12.0)

_SCG_ ::
  VFR_Waypoint
_SCG_ =
  VFR_Waypoint
    "SYDNEY CRICKET GROUND"
    (Just "NSW")
    "SCG"
    (Lat (-33) 53.5)
    (Lon 151 13.4)

_SYHD_ ::
  VFR_Waypoint
_SYHD_ =
  VFR_Waypoint
    "SYDNEY HEADS"
    (Just "NSW")
    "SYHD"
    (Lat (-33) 50.0)
    (Lon 151 17.5)

_SYP_ ::
  VFR_Waypoint
_SYP_ =
  VFR_Waypoint
    "SYMMONS PLAINS"
    (Just "TAS")
    "SYP"
    (Lat (-41) 39.3)
    (Lon 147 15.0)

_TBL_ ::
  VFR_Waypoint
_TBL_ =
  VFR_Waypoint
    "TABULAM"
    (Just "NSW")
    "TBL"
    (Lat (-28) 53.2)
    (Lon 152 34.1)

_TCH_ ::
  VFR_Waypoint
_TCH_ =
  VFR_Waypoint
    "TALC HEAD"
    (Just "NT")
    "TCH"
    (Lat (-12) 28.8)
    (Lon 130 46.0)

_TLAG_ ::
  VFR_Waypoint
_TLAG_ =
  VFR_Waypoint
    "TALLANGATTA"
    (Just "VIC")
    "TLAG"
    (Lat (-36) 13.0)
    (Lon 147 10.5)

_TLY_ ::
  VFR_Waypoint
_TLY_ =
  VFR_Waypoint
    "TALLANGATTA CAUSEWAY"
    (Just "VIC")
    "TLY"
    (Lat (-36) 12.8)
    (Lon 147 14.8)

_TRWL_ ::
  VFR_Waypoint
_TRWL_ =
  VFR_Waypoint
    "TALLARINGA WELL"
    (Just "SA")
    "TRWL"
    (Lat (-29) 2.0)
    (Lon 133 17.0)

_TLK_ ::
  VFR_Waypoint
_TLK_ =
  VFR_Waypoint
    "TALLAROOK"
    (Just "VIC")
    "TLK"
    (Lat (-37) 6.0)
    (Lon 145 6.0)

_TID_ ::
  VFR_Waypoint
_TID_ =
  VFR_Waypoint
    "TAMAR ISLAND"
    (Just "TAS")
    "TID"
    (Lat (-41) 23.0)
    (Lon 147 5.0)

_TGN_ ::
  VFR_Waypoint
_TGN_ =
  VFR_Waypoint
    "TANGORIN"
    (Just "QLD")
    "TGN"
    (Lat (-21) 44.0)
    (Lon 144 12.0)

_TUND_ ::
  VFR_Waypoint
_TUND_ =
  VFR_Waypoint
    "TANUNDA"
    (Just "SA")
    "TUND"
    (Lat (-34) 32.0)
    (Lon 138 58.0)

_TAGO_ ::
  VFR_Waypoint
_TAGO_ =
  VFR_Waypoint
    "TARAGO"
    (Just "NSW")
    "TAGO"
    (Lat (-35) 4.2)
    (Lon 149 39.3)

_TOWI_ ::
  VFR_Waypoint
_TOWI_ =
  VFR_Waypoint
    "TARCOWIE"
    (Just "SA")
    "TOWI"
    (Lat (-32) 57.0)
    (Lon 138 31.0)

_TRT_ ::
  VFR_Waypoint
_TRT_ =
  VFR_Waypoint
    "TARCUTTA"
    (Just "NSW")
    "TRT"
    (Lat (-35) 16.8)
    (Lon 147 44.3)

_TAP_ ::
  VFR_Waypoint
_TAP_ =
  VFR_Waypoint
    "TARGA GAP"
    (Just "TAS")
    "TAP"
    (Lat (-41) 18.7)
    (Lon 147 22.1)

_TAR_ ::
  VFR_Waypoint
_TAR_ =
  VFR_Waypoint
    "TARGET"
    (Just "QLD")
    "TAR"
    (Lat (-27) 36.8)
    (Lon 153 7.6)

_TLEE_ ::
  VFR_Waypoint
_TLEE_ =
  VFR_Waypoint
    "TARLEE"
    (Just "SA")
    "TLEE"
    (Lat (-34) 16.5)
    (Lon 138 46.1)

_TAS_ ::
  VFR_Waypoint
_TAS_ =
  VFR_Waypoint
    "TASMAN BRIDGE"
    (Just "TAS")
    "TAS"
    (Lat (-42) 52.0)
    (Lon 147 21.0)

_CKO_ ::
  VFR_Waypoint
_CKO_ =
  VFR_Waypoint
    "TELEGRAPH OFFICE"
    (Just "QLD")
    "CKO"
    (Lat (-19) 14.0)
    (Lon 145 29.0)

_TPL_ ::
  VFR_Waypoint
_TPL_ =
  VFR_Waypoint
    "TEMPLE BAY"
    (Just "QLD")
    "TPL"
    (Lat (-12) 16.0)
    (Lon 143 9.0)

_TLC_ ::
  VFR_Waypoint
_TLC_ =
  VFR_Waypoint
    "TERRANORA LAKES COUNTRY CLUB"
    (Just "QLD")
    "TLC"
    (Lat (-28) 12.9)
    (Lon 153 28.4)

_TWT_ ::
  VFR_Waypoint
_TWT_ =
  VFR_Waypoint
    "TEWANTIN"
    (Just "QLD")
    "TWT"
    (Lat (-26) 24.0)
    (Lon 153 2.0)

_THW_ ::
  VFR_Waypoint
_THW_ =
  VFR_Waypoint
    "THARWA"
    (Just "ACT")
    "THW"
    (Lat (-35) 30.8)
    (Lon 149 4.2)

_TCNR_ ::
  VFR_Waypoint
_TCNR_ =
  VFR_Waypoint
    "THE CORNER"
    (Just "QLD")
    "TCNR"
    (Lat (-19) 19.5)
    (Lon 146 43.7)

_THUM_ ::
  VFR_Waypoint
_THUM_ =
  VFR_Waypoint
    "THE GUMS"
    (Just "SA")
    "THUM"
    (Lat (-33) 50.0)
    (Lon 139 20.0)

_TGU_ ::
  VFR_Waypoint
_TGU_ =
  VFR_Waypoint
    "THE GUMS HS"
    (Just "SA")
    "TGU"
    (Lat (-33) 51.0)
    (Lon 139 21.0)

_TLMI_ ::
  VFR_Waypoint
_TLMI_ =
  VFR_Waypoint
    "THE LAKES MINE"
    (Just "WA")
    "TLMI"
    (Lat (-31) 51.9)
    (Lon 116 21.5)

_THK_ ::
  VFR_Waypoint
_THK_ =
  VFR_Waypoint
    "THE OAKS"
    (Just "NSW")
    "THK"
    (Lat (-34) 4.7)
    (Lon 150 34.7)

_PIN_ ::
  VFR_Waypoint
_PIN_ =
  VFR_Waypoint
    "THE PINES"
    (Just "QLD")
    "PIN"
    (Lat (-28) 8.5)
    (Lon 153 28.0)

_RCK_ ::
  VFR_Waypoint
_RCK_ =
  VFR_Waypoint
    "THE ROCK"
    (Just "NSW")
    "RCK"
    (Lat (-35) 16.5)
    (Lon 147 4.3)

_APST_ ::
  VFR_Waypoint
_APST_ =
  VFR_Waypoint
    "THE STAMFORD (HOTEL)"
    (Just "NSW")
    "APST"
    (Lat (-33) 55.9)
    (Lon 151 11.1)

_WHF_ ::
  VFR_Waypoint
_WHF_ =
  VFR_Waypoint
    "THE WHARF"
    (Just "VIC")
    "WHF"
    (Lat (-38) 6.0)
    (Lon 144 32.0)

_THB_ ::
  VFR_Waypoint
_THB_ =
  VFR_Waypoint
    "THEEBINE"
    (Just "QLD")
    "THB"
    (Lat (-25) 57.0)
    (Lon 152 33.0)

_THSM_ ::
  VFR_Waypoint
_THSM_ =
  VFR_Waypoint
    "THOMPSON 1"
    (Just "QLD")
    "THSM"
    (Lat (-17) 0.7)
    (Lon 145 45.1)

_TORN_ ::
  VFR_Waypoint
_TORN_ =
  VFR_Waypoint
    "THORNTON"
    (Just "QLD")
    "TORN"
    (Lat (-27) 49.0)
    (Lon 152 23.0)

_TNP_ ::
  VFR_Waypoint
_TNP_ =
  VFR_Waypoint
    "THORNTON GAP"
    (Just "QLD")
    "TNP"
    (Lat (-19) 21.5)
    (Lon 146 27.7)

_TBRT_ ::
  VFR_Waypoint
_TBRT_ =
  VFR_Waypoint
    "TIMBERTOP"
    (Just "QLD")
    "TBRT"
    (Lat (-16) 57.5)
    (Lon 145 48.3)

_TING_ ::
  VFR_Waypoint
_TING_ =
  VFR_Waypoint
    "TINGALPA RESERVOIR"
    (Just "QLD")
    "TING"
    (Lat (-27) 32.4)
    (Lon 153 10.0)

_TMPT_ ::
  VFR_Waypoint
_TMPT_ =
  VFR_Waypoint
    "TOM PRICE"
    (Just "WA")
    "TMPT"
    (Lat (-22) 41.7)
    (Lon 117 47.5)

_TOO_ ::
  VFR_Waypoint
_TOO_ =
  VFR_Waypoint
    "TOOBORAC"
    (Just "VIC")
    "TOO"
    (Lat (-37) 3.0)
    (Lon 144 48.0)

_TLN_ ::
  VFR_Waypoint
_TLN_ =
  VFR_Waypoint
    "TOOLLEEN"
    (Just "VIC")
    "TLN"
    (Lat (-36) 43.0)
    (Lon 144 41.0)

_TOOU_ ::
  VFR_Waypoint
_TOOU_ =
  VFR_Waypoint
    "TOOMULLA"
    (Just "QLD")
    "TOOU"
    (Lat (-19) 5.0)
    (Lon 146 28.0)

_TOGA_ ::
  VFR_Waypoint
_TOGA_ =
  VFR_Waypoint
    "TOORONGA"
    (Just "VIC")
    "TOGA"
    (Lat (-37) 50.9)
    (Lon 145 2.8)

_TOWA_ ::
  VFR_Waypoint
_TOWA_ =
  VFR_Waypoint
    "TOWRANA HS"
    (Just "WA")
    "TOWA"
    (Lat (-25) 26.0)
    (Lon 115 14.0)

_TVM_ ::
  VFR_Waypoint
_TVM_ =
  VFR_Waypoint
    "TREVALLYN DAM"
    (Just "TAS")
    "TVM"
    (Lat (-41) 27.3)
    (Lon 147 5.3)

_TRIN_ ::
  VFR_Waypoint
_TRIN_ =
  VFR_Waypoint
    "TRINITY BEACH"
    (Just "QLD")
    "TRIN"
    (Lat (-16) 47.0)
    (Lon 145 42.0)

_TRUO_ ::
  VFR_Waypoint
_TRUO_ =
  VFR_Waypoint
    "TRURO"
    (Just "SA")
    "TRUO"
    (Lat (-34) 24.7)
    (Lon 139 7.2)

_TKER_ ::
  VFR_Waypoint
_TKER_ =
  VFR_Waypoint
    "TUCKER"
    (Just "QLD")
    "TKER"
    (Lat (-24) 29.0)
    (Lon 149 12.0)

_TUK_ ::
  VFR_Waypoint
_TUK_ =
  VFR_Waypoint
    "TUCKERS KNOB"
    (Just "NSW")
    "TUK"
    (Lat (-30) 20.6)
    (Lon 152 58.8)

_TGC_ ::
  VFR_Waypoint
_TGC_ =
  VFR_Waypoint
    "TUGGERANONG TOWN CENTRE"
    (Just "ACT")
    "TGC"
    (Lat (-35) 25.0)
    (Lon 149 4.0)

_TUMB_ ::
  VFR_Waypoint
_TUMB_ =
  VFR_Waypoint
    "TUMBULGUM"
    (Just "NSW")
    "TUMB"
    (Lat (-28) 16.7)
    (Lon 153 28.0)

_TTLE_ ::
  VFR_Waypoint
_TTLE_ =
  VFR_Waypoint
    "TURTLE PT"
    (Just "NT")
    "TTLE"
    (Lat (-14) 51.0)
    (Lon 129 15.0)

_TWRN_ ::
  VFR_Waypoint
_TWRN_ =
  VFR_Waypoint
    "TWO RN"
    (Just "NSW")
    "TWRN"
    (Lat (-33) 56.2)
    (Lon 150 53.3)

_TOS_ ::
  VFR_Waypoint
_TOS_ =
  VFR_Waypoint
    "TWO ROCKS"
    (Just "WA")
    "TOS"
    (Lat (-31) 29.5)
    (Lon 115 35.0)

_UKER_ ::
  VFR_Waypoint
_UKER_ =
  VFR_Waypoint
    "UKEREBAGH ISLAND"
    (Just "NSW")
    "UKER"
    (Lat (-28) 10.8)
    (Lon 153 32.8)

_UDA_ ::
  VFR_Waypoint
_UDA_ =
  VFR_Waypoint
    "ULLADULLA"
    (Just "NSW")
    "UDA"
    (Lat (-35) 21.0)
    (Lon 150 29.0)

_UNDW_ ::
  VFR_Waypoint
_UNDW_ =
  VFR_Waypoint
    "UNDEMOW WATERHOLE"
    (Just "NT")
    "UNDW"
    (Lat (-17) 35.0)
    (Lon 135 15.0)

_UQLD_ ::
  VFR_Waypoint
_UQLD_ =
  VFR_Waypoint
    "UNI OF QLD ST LUCIA"
    (Just "QLD")
    "UQLD"
    (Lat (-27) 29.9)
    (Lon 153 0.8)

_USW_ ::
  VFR_Waypoint
_USW_ =
  VFR_Waypoint
    "UPPER SWAN"
    (Just "WA")
    "USW"
    (Lat (-31) 46.3)
    (Lon 116 1.0)

_URAN_ ::
  VFR_Waypoint
_URAN_ =
  VFR_Waypoint
    "URANGAN"
    (Just "QLD")
    "URAN"
    (Lat (-25) 17.0)
    (Lon 152 54.0)

_URA_ ::
  VFR_Waypoint
_URA_ =
  VFR_Waypoint
    "URANQUINTY"
    (Just "NSW")
    "URA"
    (Lat (-35) 11.5)
    (Lon 147 14.8)

_URC_ ::
  VFR_Waypoint
_URC_ =
  VFR_Waypoint
    "URRBRAE AGRICULTURAL HIGH SCHOOL"
    (Just "SA")
    "URC"
    (Lat (-34) 58.0)
    (Lon 138 37.5)

_URU_ ::
  VFR_Waypoint
_URU_ =
  VFR_Waypoint
    "URUNGA"
    (Just "NSW")
    "URU"
    (Lat (-30) 29.5)
    (Lon 153 1.0)

_VTG_ ::
  VFR_Waypoint
_VTG_ =
  VFR_Waypoint
    "VEHICLE TESTING GROUND"
    (Just "VIC")
    "VTG"
    (Lat (-37) 53.0)
    (Lon 144 25.0)

_VELO_ ::
  VFR_Waypoint
_VELO_ =
  VFR_Waypoint
    "VELODROME"
    (Just "SA")
    "VELO"
    (Lat (-34) 50.6)
    (Lon 138 36.7)

_VICP_ ::
  VFR_Waypoint
_VICP_ =
  VFR_Waypoint
    "VICTORIA PARK"
    (Just "SA")
    "VICP"
    (Lat (-34) 56.0)
    (Lon 138 37.3)

_VOKH_ ::
  VFR_Waypoint
_VOKH_ =
  VFR_Waypoint
    "VOKES HILL"
    (Just "SA")
    "VOKH"
    (Lat (-28) 29.0)
    (Lon 130 35.0)

_VPH_ ::
  VFR_Waypoint
_VPH_ =
  VFR_Waypoint
    "VPH"
    (Just "WA")
    "VPH"
    (Lat (-31) 56.7)
    (Lon 115 57.6)

_WGL_ ::
  VFR_Waypoint
_WGL_ =
  VFR_Waypoint
    "WAIGEN LAKES"
    (Just "WA")
    "WGL"
    (Lat (-27) 37.0)
    (Lon 128 47.0)

_WTC_ ::
  VFR_Waypoint
_WTC_ =
  VFR_Waypoint
    "WAITE CAMPUS"
    (Just "SA")
    "WTC"
    (Lat (-34) 58.2)
    (Lon 138 38.1)

_WKT_ ::
  VFR_Waypoint
_WKT_ =
  VFR_Waypoint
    "WALKERSTON"
    (Just "QLD")
    "WKT"
    (Lat (-21) 9.8)
    (Lon 149 3.8)

_WBH_ ::
  VFR_Waypoint
_WBH_ =
  VFR_Waypoint
    "WALLABADAH"
    (Just "NSW")
    "WBH"
    (Lat (-31) 32.3)
    (Lon 150 49.5)

_WAN_ ::
  VFR_Waypoint
_WAN_ =
  VFR_Waypoint
    "WALLAN"
    (Just "VIC")
    "WAN"
    (Lat (-37) 24.5)
    (Lon 144 58.6)

_WGR_ ::
  VFR_Waypoint
_WGR_ =
  VFR_Waypoint
    "WALLANGARRA"
    (Just "NSW")
    "WGR"
    (Lat (-28) 55.0)
    (Lon 151 56.0)

_WRO_ ::
  VFR_Waypoint
_WRO_ =
  VFR_Waypoint
    "WALLAROO"
    (Just "SA")
    "WRO"
    (Lat (-33) 56.0)
    (Lon 137 38.0)

_WMB_ ::
  VFR_Waypoint
_WMB_ =
  VFR_Waypoint
    "WALLUMBILLA"
    (Just "QLD")
    "WMB"
    (Lat (-26) 35.0)
    (Lon 149 11.0)

_WPE_ ::
  VFR_Waypoint
_WPE_ =
  VFR_Waypoint
    "WALPOLE"
    (Just "WA")
    "WPE"
    (Lat (-34) 59.0)
    (Lon 116 44.0)

_WTBG_ ::
  VFR_Waypoint
_WTBG_ =
  VFR_Waypoint
    "WALTER TAYLOR BRIDGE"
    (Just "QLD")
    "WTBG"
    (Lat (-27) 30.3)
    (Lon 152 58.4)

_WDN_ ::
  VFR_Waypoint
_WDN_ =
  VFR_Waypoint
    "WANDANDIAN"
    (Just "NSW")
    "WDN"
    (Lat (-35) 5.0)
    (Lon 150 31.0)

_WAND_ ::
  VFR_Waypoint
_WAND_ =
  VFR_Waypoint
    "WANDERING"
    (Just "WA")
    "WAND"
    (Lat (-32) 40.5)
    (Lon 116 40.0)

_WAT_ ::
  VFR_Waypoint
_WAT_ =
  VFR_Waypoint
    "WANGETTI"
    (Just "QLD")
    "WAT"
    (Lat (-16) 39.9)
    (Lon 145 34.0)

_WAG_ ::
  VFR_Waypoint
_WAG_ =
  VFR_Waypoint
    "WANTABADGERY"
    (Just "NSW")
    "WAG"
    (Lat (-35) 3.5)
    (Lon 147 43.3)

_WRNA_ ::
  VFR_Waypoint
_WRNA_ =
  VFR_Waypoint
    "WAROONA"
    (Just "WA")
    "WRNA"
    (Lat (-32) 50.5)
    (Lon 115 55.0)

_WAD_ ::
  VFR_Waypoint
_WAD_ =
  VFR_Waypoint
    "WARRAGAMBA DAM"
    (Just "NSW")
    "WAD"
    (Lat (-33) 53.1)
    (Lon 150 35.5)

_WASL_ ::
  VFR_Waypoint
_WASL_ =
  VFR_Waypoint
    "WARRAL SILO"
    (Just "NSW")
    "WASL"
    (Lat (-31) 9.0)
    (Lon 150 51.5)

_WRD_ ::
  VFR_Waypoint
_WRD_ =
  VFR_Waypoint
    "WARRANDYTE"
    (Just "VIC")
    "WRD"
    (Lat (-37) 45.0)
    (Lon 145 12.5)

_WRR_ ::
  VFR_Waypoint
_WRR_ =
  VFR_Waypoint
    "WARREN RESERVOIR"
    (Just "SA")
    "WRR"
    (Lat (-34) 42.5)
    (Lon 138 56.0)

_WFM_ ::
  VFR_Waypoint
_WFM_ =
  VFR_Waypoint
    "WARWICK FARM"
    (Just "NSW")
    "WFM"
    (Lat (-33) 54.7)
    (Lon 150 56.8)

_WFL_ ::
  VFR_Waypoint
_WFL_ =
  VFR_Waypoint
    "WATERFALL"
    (Just "NSW")
    "WFL"
    (Lat (-34) 8.2)
    (Lon 150 59.5)

_WAYS_ ::
  VFR_Waypoint
_WAYS_ =
  VFR_Waypoint
    "WAYSIDE"
    (Just "NT")
    "WAYS"
    (Lat (-15) 34.5)
    (Lon 131 2.4)

_WWN_ ::
  VFR_Waypoint
_WWN_ =
  VFR_Waypoint
    "WEALWANDANGIE"
    (Just "QLD")
    "WWN"
    (Lat (-24) 25.0)
    (Lon 148 3.0)

_WCP_ ::
  VFR_Waypoint
_WCP_ =
  VFR_Waypoint
    "WELLCAMP DOWNS"
    (Just "QLD")
    "WCP"
    (Lat (-27) 33.0)
    (Lon 151 51.0)

_WELL_ ::
  VFR_Waypoint
_WELL_ =
  VFR_Waypoint
    "WELLINGTON PT"
    (Just "QLD")
    "WELL"
    (Lat (-27) 28.1)
    (Lon 153 14.5)

_WLS_ ::
  VFR_Waypoint
_WLS_ =
  VFR_Waypoint
    "WELLSHOT"
    (Just "QLD")
    "WLS"
    (Lat (-23) 54.0)
    (Lon 144 26.0)

_WELS_ ::
  VFR_Waypoint
_WELS_ =
  VFR_Waypoint
    "WELSHPOOL"
    (Just "VIC")
    "WELS"
    (Lat (-38) 39.9)
    (Lon 146 26.3)

_WBER_ ::
  VFR_Waypoint
_WBER_ =
  VFR_Waypoint
    "WERRIBEE RACECOURSE"
    (Just "VIC")
    "WBER"
    (Lat (-37) 54.0)
    (Lon 144 38.5)

_WBES_ ::
  VFR_Waypoint
_WBES_ =
  VFR_Waypoint
    "WERRIBEE SOUTH"
    (Just "VIC")
    "WBES"
    (Lat (-37) 58.6)
    (Lon 144 41.3)

_WEK_ ::
  VFR_Waypoint
_WEK_ =
  VFR_Waypoint
    "WERRIS CREEK"
    (Just "NSW")
    "WEK"
    (Lat (-31) 21.5)
    (Lon 150 39.0)

_WSM_ ::
  VFR_Waypoint
_WSM_ =
  VFR_Waypoint
    "WEST ARM"
    (Just "NT")
    "WSM"
    (Lat (-12) 33.0)
    (Lon 130 47.3)

_WEBS_ ::
  VFR_Waypoint
_WEBS_ =
  VFR_Waypoint
    "WEST BASS"
    (Just "VIC")
    "WEBS"
    (Lat (-39) 30.0)
    (Lon 141 0.0)

_WTG_ ::
  VFR_Waypoint
_WTG_ =
  VFR_Waypoint
    "WEST GAP"
    (Just "TAS")
    "WTG"
    (Lat (-41) 20.7)
    (Lon 146 46.5)

_WSN_ ::
  VFR_Waypoint
_WSN_ =
  VFR_Waypoint
    "WEST LAGOON"
    (Just "TAS")
    "WSN"
    (Lat (-41) 36.2)
    (Lon 147 1.7)

_WEP_ ::
  VFR_Waypoint
_WEP_ =
  VFR_Waypoint
    "WEST PT"
    (Just "QLD")
    "WEP"
    (Lat (-19) 7.7)
    (Lon 146 46.7)

_WNFE_ ::
  VFR_Waypoint
_WNFE_ =
  VFR_Waypoint
    "WESTERN FREEWAY"
    (Just "QLD")
    "WNFE"
    (Lat (-27) 34.5)
    (Lon 152 56.7)

_WES_ ::
  VFR_Waypoint
_WES_ =
  VFR_Waypoint
    "WESTGATE BRIDGE"
    (Just "VIC")
    "WES"
    (Lat (-37) 49.8)
    (Lon 144 53.8)

_WMR_ ::
  VFR_Waypoint
_WMR_ =
  VFR_Waypoint
    "WESTMAR"
    (Just "QLD")
    "WMR"
    (Lat (-27) 55.0)
    (Lon 149 43.0)

_WST_ ::
  VFR_Waypoint
_WST_ =
  VFR_Waypoint
    "WESTMEAD"
    (Just "NSW")
    "WST"
    (Lat (-33) 48.2)
    (Lon 150 59.2)

_WTB_ ::
  VFR_Waypoint
_WTB_ =
  VFR_Waypoint
    "WETHERBY"
    (Just "QLD")
    "WTB"
    (Lat (-21) 30.0)
    (Lon 142 50.0)

_WHM_ ::
  VFR_Waypoint
_WHM_ =
  VFR_Waypoint
    "WHIM CREEK"
    (Just "WA")
    "WHM"
    (Lat (-20) 50.0)
    (Lon 117 50.0)

_WTRK_ ::
  VFR_Waypoint
_WTRK_ =
  VFR_Waypoint
    "WHITE ROCK"
    (Just "QLD")
    "WTRK"
    (Lat (-18) 46.7)
    (Lon 146 43.1)

_WHW_ ::
  VFR_Waypoint
_WHW_ =
  VFR_Waypoint
    "WHITEWOOD"
    (Just "QLD")
    "WHW"
    (Lat (-21) 29.0)
    (Lon 143 36.0)

_WTS_ ::
  VFR_Waypoint
_WTS_ =
  VFR_Waypoint
    "WHITTLESEA"
    (Just "VIC")
    "WTS"
    (Lat (-37) 31.0)
    (Lon 145 7.0)

_WIKP_ ::
  VFR_Waypoint
_WIKP_ =
  VFR_Waypoint
    "WICKHAM PT"
    (Just "NT")
    "WIKP"
    (Lat (-12) 30.3)
    (Lon 130 51.6)

_WHPL_ ::
  VFR_Waypoint
_WHPL_ =
  VFR_Waypoint
    "WILD HORSE PLAINS"
    (Just "SA")
    "WHPL"
    (Lat (-34) 21.6)
    (Lon 138 17.3)

_WILE_ ::
  VFR_Waypoint
_WILE_ =
  VFR_Waypoint
    "WILLEROO"
    (Just "NT")
    "WILE"
    (Lat (-15) 17.0)
    (Lon 131 33.9)

_WMS_ ::
  VFR_Waypoint
_WMS_ =
  VFR_Waypoint
    "WILLIAMSTOWN"
    (Just "VIC")
    "WMS"
    (Lat (-37) 52.2)
    (Lon 144 54.7)

_WICK_ ::
  VFR_Waypoint
_WICK_ =
  VFR_Waypoint
    "WILLIE CREEK"
    (Just "WA")
    "WICK"
    (Lat (-17) 45.0)
    (Lon 122 12.5)

_WIE_ ::
  VFR_Waypoint
_WIE_ =
  VFR_Waypoint
    "WIMMERA"
    (Just "NSW")
    "WIE"
    (Lat (-31) 9.5)
    (Lon 150 49.0)

_WDU_ ::
  VFR_Waypoint
_WDU_ =
  VFR_Waypoint
    "WIRRADGURIE"
    (Just "NSW")
    "WDU"
    (Lat (-31) 54.0)
    (Lon 152 4.0)

_WSFR_ ::
  VFR_Waypoint
_WSFR_ =
  VFR_Waypoint
    "WISEMANS FERRY"
    (Just "NSW")
    "WSFR"
    (Lat (-33) 22.8)
    (Lon 150 59.3)

_WTHB_ ::
  VFR_Waypoint
_WTHB_ =
  VFR_Waypoint
    "WITHNELL BAY"
    (Just "WA")
    "WTHB"
    (Lat (-20) 34.8)
    (Lon 116 47.2)

_WHDW_ ::
  VFR_Waypoint
_WHDW_ =
  VFR_Waypoint
    "WIVENHOE DAM WALL"
    (Just "QLD")
    "WHDW"
    (Lat (-27) 23.7)
    (Lon 152 36.5)

_WBK_ ::
  VFR_Waypoint
_WBK_ =
  VFR_Waypoint
    "WOODBROOK"
    (Just "WA")
    "WBK"
    (Lat (-20) 54.7)
    (Lon 117 7.0)

_WOT_ ::
  VFR_Waypoint
_WOT_ =
  VFR_Waypoint
    "WOODGATE"
    (Just "QLD")
    "WOT"
    (Lat (-25) 7.0)
    (Lon 152 34.0)

_WOOS_ ::
  VFR_Waypoint
_WOOS_ =
  VFR_Waypoint
    "WOODLANDS GOLF COURSE"
    (Just "VIC")
    "WOOS"
    (Lat (-37) 59.8)
    (Lon 145 6.0)

_WMP_ ::
  VFR_Waypoint
_WMP_ =
  VFR_Waypoint
    "WOODMAN PT"
    (Just "WA")
    "WMP"
    (Lat (-32) 8.5)
    (Lon 115 44.0)

_WYPT_ ::
  VFR_Waypoint
_WYPT_ =
  VFR_Waypoint
    "WOODY PT"
    (Just "QLD")
    "WYPT"
    (Lat (-27) 15.9)
    (Lon 153 6.2)

_WGG_ ::
  VFR_Waypoint
_WGG_ =
  VFR_Waypoint
    "WOOLGOOLGA"
    (Just "NSW")
    "WGG"
    (Lat (-30) 6.8)
    (Lon 153 12.0)

_WOI_ ::
  VFR_Waypoint
_WOI_ =
  VFR_Waypoint
    "WOOLLAMIA"
    (Just "NSW")
    "WOI"
    (Lat (-35) 1.2)
    (Lon 150 39.6)

_WNK_ ::
  VFR_Waypoint
_WNK_ =
  VFR_Waypoint
    "WOOMANOOKA"
    (Just "QLD")
    "WNK"
    (Lat (-13) 44.0)
    (Lon 141 35.0)

_WAM_ ::
  VFR_Waypoint
_WAM_ =
  VFR_Waypoint
    "WOOMARGAMA"
    (Just "NSW")
    "WAM"
    (Lat (-35) 50.0)
    (Lon 147 14.9)

_WRM_ ::
  VFR_Waypoint
_WRM_ =
  VFR_Waypoint
    "WOORIM"
    (Just "QLD")
    "WRM"
    (Lat (-27) 4.7)
    (Lon 153 12.2)

_WUN_ ::
  VFR_Waypoint
_WUN_ =
  VFR_Waypoint
    "WUNDOWIE"
    (Just "WA")
    "WUN"
    (Lat (-31) 46.0)
    (Lon 116 22.6)

_WUTUL_ ::
  VFR_Waypoint
_WUTUL_ =
  VFR_Waypoint
    "WUTUL"
    (Just "QLD")
    "WUTUL"
    (Lat (-27) 1.8)
    (Lon 151 48.5)

_WMF_ ::
  VFR_Waypoint
_WMF_ =
  VFR_Waypoint
    "WYMAH FERRY"
    (Just "NSW")
    "WMF"
    (Lat (-36) 2.5)
    (Lon 147 15.8)

_WYR_ ::
  VFR_Waypoint
_WYR_ =
  VFR_Waypoint
    "WYREEMA"
    (Just "QLD")
    "WYR"
    (Lat (-27) 39.0)
    (Lon 151 51.5)

_YMA_ ::
  VFR_Waypoint
_YMA_ =
  VFR_Waypoint
    "YAAMBA"
    (Just "QLD")
    "YMA"
    (Lat (-23) 8.0)
    (Lon 150 22.0)

_YBU_ ::
  VFR_Waypoint
_YBU_ =
  VFR_Waypoint
    "YABULU"
    (Just "QLD")
    "YBU"
    (Lat (-19) 12.8)
    (Lon 146 35.8)

_YKH_ ::
  VFR_Waypoint
_YKH_ =
  VFR_Waypoint
    "YACKANDANDAH"
    (Just "VIC")
    "YKH"
    (Lat (-36) 18.7)
    (Lon 146 50.5)

_ALBO_ ::
  VFR_Waypoint
_ALBO_ =
  VFR_Waypoint
    "YALBOROO"
    (Just "QLD")
    "ALBO"
    (Lat (-20) 50.0)
    (Lon 148 39.0)

_YYN_ ::
  VFR_Waypoint
_YYN_ =
  VFR_Waypoint
    "YAN YEAN RESV"
    (Just "VIC")
    "YYN"
    (Lat (-37) 33.5)
    (Lon 145 8.3)

_ANDA_ ::
  VFR_Waypoint
_ANDA_ =
  VFR_Waypoint
    "YANDARAN"
    (Just "QLD")
    "ANDA"
    (Lat (-24) 43.0)
    (Lon 152 7.0)

_YNA_ ::
  VFR_Waypoint
_YNA_ =
  VFR_Waypoint
    "YANDINA"
    (Just "QLD")
    "YNA"
    (Lat (-26) 33.8)
    (Lon 152 57.3)

_YGB_ ::
  VFR_Waypoint
_YGB_ =
  VFR_Waypoint
    "YANGEBUP LAKE"
    (Just "WA")
    "YGB"
    (Lat (-32) 7.2)
    (Lon 115 50.0)

_YYM_ ::
  VFR_Waypoint
_YYM_ =
  VFR_Waypoint
    "YARINGA YACHT MARINA"
    (Just "VIC")
    "YYM"
    (Lat (-38) 14.8)
    (Lon 145 15.1)

_YBH_ ::
  VFR_Waypoint
_YBH_ =
  VFR_Waypoint
    "YARRABAH"
    (Just "QLD")
    "YBH"
    (Lat (-16) 55.0)
    (Lon 145 53.0)

_ASST_ ::
  VFR_Waypoint
_ASST_ =
  VFR_Waypoint
    "YASS TOWNSHIP"
    (Just "NSW")
    "ASST"
    (Lat (-34) 51.0)
    (Lon 148 55.0)

_ORKT_ ::
  VFR_Waypoint
_ORKT_ =
  VFR_Waypoint
    "YORK TOWNSHIP"
    (Just "WA")
    "ORKT"
    (Lat (-31) 53.0)
    (Lon 116 46.0)

_YKS_ ::
  VFR_Waypoint
_YKS_ =
  VFR_Waypoint
    "YORKEYS KNOB"
    (Just "QLD")
    "YKS"
    (Lat (-16) 48.6)
    (Lon 145 43.5)

_ORNN_ ::
  VFR_Waypoint
_ORNN_ =
  VFR_Waypoint
    "YORNANING TOWNSHIP"
    (Just "WA")
    "ORNN"
    (Lat (-32) 44.0)
    (Lon 117 10.0)

_ULAB_ ::
  VFR_Waypoint
_ULAB_ =
  VFR_Waypoint
    "YULABILLA"
    (Just "QLD")
    "ULAB"
    (Lat (-27) 4.0)
    (Lon 149 42.0)

_URAR_ ::
  VFR_Waypoint
_URAR_ =
  VFR_Waypoint
    "YURARABA"
    (Just "QLD")
    "URAR"
    (Lat (-28) 20.0)
    (Lon 151 24.0)

_ZIN_ ::
  VFR_Waypoint
_ZIN_ =
  VFR_Waypoint
    "ZUIZIN ISLAND"
    (Just "QLD")
    "ZIN"
    (Lat (-10) 6.0)
    (Lon 143 20.0)

all_VFR_Waypoint ::
  VFR_Waypoints
all_VFR_Waypoint =
  VFR_Waypoints
    [
      _KNO_
    , _ABKL_
    , _TVT_
    , _ACE_
    , _ANI_
    , _ACLD_
    , _ACTY_
    , _ADB_
    , _AOG_
    , _ADI_
    , _ADWD_
    , _ALON_
    , _APL_
    , _AKW_
    , _ALBA_
    , _ALOA_
    , _ALTS_
    , _ANG_
    , _APM_
    , _ANA_
    , _ANP_
    , _APPN_
    , _ARCD_
    , _AEN_
    , _ARE_
    , _AWP_
    , _ASU_
    , _ATN_
    , _ATG_
    , _ATV_
    , _ALEC_
    , _AVCA_
    , _BADA_
    , _BMP_
    , _BKRL_
    , _BLHS_
    , _BOA_
    , _BANG_
    , _BDT_
    , _BARB_
    , _BHCP_
    , _BSL_
    , _BRNJ_
    , _BRR_
    , _BRGE_
    , _BPN_
    , _KALK_
    , _BATB_
    , _BTI_
    , _BNE_
    , _BAW_
    , _BCM_
    , _BIV_
    , _BENT_
    , _BCH_
    , _BLR_
    , _BLIG_
    , _BENV_
    , _BND_
    , _BEE_
    , _BESI_
    , _BVG_
    , _BDTN_
    , _BIBA_
    , _BDWD_
    , _BCT_
    , _BKM_
    , _BKIS_
    , _BLIC_
    , _BTM_
    , _BOAT_
    , _BODD_
    , _BGN_
    , _BLTB_
    , _BSP_
    , _BUVY_
    , _BNG_
    , _BONG_
    , _BOON_
    , _BOAR_
    , _BBH_
    , _BOWEN_
    , _BOWB_
    , _BWV_
    , _BWMS_
    , _BOW_
    , _BWL_
    , _BOV_
    , _BPI_
    , _BAXT_
    , _BZA_
    , _BBBG_
    , _BBI_
    , _BTO_
    , _BTJ_
    , _BRY_
    , _BRIN_
    , _BCTY_
    , _OGABA_
    , _BDF_
    , _BYN_
    , _BBG_
    , _BTON_
    , _BRI_
    , _OBSTM_
    , _BRH_
    , _BUCP_
    , _BUCN_
    , _BKD_
    , _BPK_
    , _BDH_
    , _BEN_
    , _BUG_
    , _URG_
    , _BLGH_
    , _BUB_
    , _BURR_
    , _BUGA_
    , _BJK_
    , _BMK_
    , _BYFD_
    , _BYNO_
    , _BYRK_
    , _BBAY_
    , _CBLT_
    , _CABO_
    , _CALEN_
    , _CRDZ_
    , _CALT_
    , _CBRA_
    , _CPA_
    , _CLLN_
    , _CAMB_
    , _RCSE_
    , _CNB_
    , _CDM_
    , _CAV_
    , _CBY_
    , _CAPS_
    , _CCL_
    , _CPY_
    , _CGR_
    , _CGF_
    , _CPH_
    , _CAJE_
    , _CAKE_
    , _CMB_
    , _CPMN_
    , _CPLD_
    , _CPHE_
    , _CAPT_
    , _CAU_
    , _CARE_
    , _CDNA_
    , _CARIN_
    , _CIK_
    , _CARR_
    , _CLJ_
    , _CTT_
    , _CRPT_
    , _CCP_
    , _CAO_
    , _CBRG_
    , _CERB_
    , _CYM_
    , _CHI_
    , _CHAP_
    , _CHAT_
    , _CHN_
    , _CIB_
    , _COY_
    , _CNTH_
    , _CSTH_
    , _CWST_
    , _CIH_
    , _CYB_
    , _CEN_
    , _CVD_
    , _CFI_
    , _CLS_
    , _CGH_
    , _CLOY_
    , _COBA_
    , _CKT_
    , _CBI_
    , _CGE_
    , _CONG_
    , _CJN_
    , _CGM_
    , _CBYC_
    , _CIS_
    , _CLMN_
    , _CMDR_
    , _COOL_
    , _CORO_
    , _CPL_
    , _CPNG_
    , _CVR_
    , _COSS_
    , _CTE_
    , _COWI_
    , _COWR_
    , _CML_
    , _CBV_
    , _CGB_
    , _CRAY_
    , _CREM_
    , _CES_
    , _CWK_
    , _CUL_
    , _CRPC_
    , _CNT_
    , _CGD_
    , _CCN_
    , _DAIN_
    , _DLMO_
    , _DRY_
    , _DMW_
    , _DARL_
    , _DHH_
    , _DND_
    , _DBO_
    , _DFD_
    , _DGY_
    , _DSS_
    , _DPW_
    , _DEL_
    , _DNP_
    , _DWB_
    , _DVM_
    , _DIBE_
    , _DODI_
    , _DLPT_
    , _DSN_
    , _DOP_
    , _DOOM_
    , _DRO_
    , _DCIS_
    , _DOU_
    , _DLP_
    , _DBPT_
    , _DRLD_
    , _DRM_
    , _DRP_
    , _DRN_
    , _DCRK_
    , _DAG_
    , _DUB_
    , _DLY_
    , _DGN_
    , _DUWN_
    , _DUA_
    , _DMT_
    , _DONG_
    , _DTON_
    , _EAN_
    , _EARH_
    , _EARV_
    , _EAM_
    , _EGT_
    , _ETP_
    , _EDP_
    , _EDT_
    , _ELDO_
    , _ERB_
    , _EMY_
    , _EDOR_
    , _EPPG_
    , _ERSK_
    , _ETON_
    , _EMI_
    , _EWI_
    , _EMD_
    , _FCP_
    , _FHS_
    , _FDP_
    , _FNVL_
    , _FLDI_
    , _FISH_
    , _FIT_
    , _FID_
    , _FGN_
    , _FPK_
    , _FYN_
    , _FOOT_
    , _FMN_
    , _FDL_
    , _FFLD_
    , _FOWB_
    , _FRLG_
    , _FWO_
    , _FRE_
    , _FREM_
    , _FRWV_
    , _GALGA_
    , _GGN_
    , _GAE_
    , _GWTR_
    , _GST_
    , _GWT_
    , _GWB_
    , _GEK_
    , _GRB_
    , _GEP_
    , _GOY_
    , _GNR_
    , _GIM_
    , _GIRU_
    , _GVB_
    , _GBRY_
    , _GBR_
    , _GLEN_
    , _GMN_
    , _GLC_
    , _GRE_
    , _GRK_
    , _GCR_
    , _GMH_
    , _GOI_
    , _GON_
    , _GGV_
    , _GMBG_
    , _GNN_
    , _GGO_
    , _GOV_
    , _GOW_
    , _GRAA_
    , _GVH_
    , _GRHL_
    , _GNIS_
    , _GRL_
    , _GRNH_
    , _GNM_
    , _GRRV_
    , _GDG_
    , _GUP_
    , _GUNN_
    , _GUNG_
    , _HADEN_
    , _HSP_
    , _HALL_
    , _HMM_
    , _HMN_
    , _HAVY_
    , _HBB_
    , _HAF_
    , _HGTE_
    , _HARV_
    , _HSTI_
    , _HASS_
    , _HATF_
    , _HVI_
    , _HPT_
    , _HAZ_
    , _HEAT_
    , _HRR_
    , _HNB_
    , _HED_
    , _HENTY_
    , _HKE_
    , _HXB_
    , _HIPA_
    , _HVTG_
    , _HIM_
    , _HNCH_
    , _HDWL_
    , _HCTY_
    , _HBKT_
    , _HOLM_
    , _HBU_
    , _HDP_
    , _HPI_
    , _HVR_
    , _HORD_
    , _HZWF_
    , _HBVT_
    , _HSY_
    , _HST_
    , _HWG_
    , _HGR_
    , _HWW_
    , _HYH_
    , _HRD_
    , _HYDEN_
    , _IND_
    , _IDNA_
    , _IDK_
    , _INGL_
    , _IPHL_
    , _ISB_
    , _JSK_
    , _JAC_
    , _JNR_
    , _JSL_
    , _JACW_
    , _JMPP_
    , _JADL_
    , _JCK_
    , _JEA_
    , _JES_
    , _JIBN_
    , _JIA_
    , _JDN_
    , _JGK_
    , _JPP_
    , _JUNEE_
    , _JUP_
    , _KKN_
    , _KAO_
    , _KTS_
    , _KEP_
    , _KERW_
    , _KALL_
    , _KSI_
    , _KMA_
    , _KIAN_
    , _KDBF_
    , _KIEWA_
    , _KLCY_
    , _KKV_
    , _KIM_
    , _KMG_
    , _KLTO_
    , _KCAS_
    , _KRT_
    , _KGLE_
    , _KBCH_
    , _KCFF_
    , _KGT_
    , _KINN_
    , _KIRA_
    , _KSPT_
    , _KNW_
    , _KTG_
    , _KBD_
    , _KOOM_
    , _KANC_
    , _KOT_
    , _KOPP_
    , _KREE_
    , _KUN_
    , _KRN_
    , _KRMD_
    , _KYP_
    , _KYE_
    , _KTN_
    , _LDH_
    , _LDLY_
    , _LKT_
    , _LKAD_
    , _LBT_
    , _LKON_
    , _LCR_
    , _LCDI_
    , _LAD_
    , _LDAP_
    , _LKEC_
    , _LEM_
    , _LEYN_
    , _LFS_
    , _LFRO_
    , _LGGN_
    , _LGGS_
    , _LGEN_
    , _LGDA_
    , _LKH_
    , _LKIM_
    , _LKG_
    , _LMC_
    , _LME_
    , _LMWL_
    , _LOOH_
    , _LKNL_
    , _LRID_
    , _LRAN_
    , _LSPR_
    , _LTOM_
    , _LTL_
    , _LTRR_
    , _WITE_
    , _LYEO_
    , _LKEE_
    , _LCD_
    , _LANC_
    , _LGH_
    , _LHCK_
    , _LSDW_
    , _LUY_
    , _LAUD_
    , _LVAB_
    , _TON_
    , _LWG_
    , _LAOS_
    , _LPT_
    , _LPD_
    , _LIHR_
    , _LIY_
    , _LOWS_
    , _LVE_
    , _LMGE_
    , _LRM_
    , _LBET_
    , _LSR_
    , _LOGI_
    , _LRF_
    , _LFC_
    , _LORN_
    , _LHD_
    , _LWI_
    , _LRP_
    , _LWD_
    , _LNDA_
    , _LYNR_
    , _MACB_
    , _MZR_
    , _MKV_
    , _MGEW_
    , _MAS_
    , _MADO_
    , _MDU_
    , _MGL_
    , _MOP_
    , _MAL_
    , _MANLY_
    , _MTD_
    , _MBA_
    , _MRL_
    , _MLIT_
    , _MARQ_
    , _MRBR_
    , _MAR_
    , _MARR_
    , _MHT_
    , _MKN_
    , _MVL_
    , _MHD_
    , _MRH_
    , _MAYD_
    , _MYF_
    , _MAYL_
    , _MVAL_
    , _MCTY_
    , _MCG_
    , _MELS_
    , _MEG_
    , _MRI_
    , _MRJ_
    , _MIAR_
    , _MHGO_
    , _MII_
    , _MCRO_
    , _MSTM_
    , _MPO_
    , _MIJ_
    , _MISB_
    , _MSC_
    , _MITI_
    , _MFBH_
    , _MFH_
    , _MBH_
    , _MVC_
    , _MIE_
    , _MPSC_
    , _MORN_
    , _MSV_
    , _MMA_
    , _BAK_
    , _MBST_
    , _MBC_
    , _MBK_
    , _MTBL_
    , _MBR_
    , _MTB_
    , _MCAR_
    , _MCHR_
    , _MCOM_
    , _MTK_
    , _MTC_
    , _MTBA_
    , _TCR_
    , _MCOO_
    , _MCOT_
    , _MUE_
    , _MDY_
    , _MELE_
    , _MEV_
    , _MFN_
    , _MGLO_
    , _MVT_
    , _MJK_
    , _MTKI_
    , _MLI_
    , _MLUY_
    , _MCD_
    , _MTMA_
    , _MMY_
    , _MOB_
    , _MGN_
    , _MTM_
    , _MIY_
    , _OOR_
    , _MPG_
    , _PIPR_
    , _MPT_
    , _SOV_
    , _MTEW_
    , _MUM_
    , _MUSD_
    , _MTLR_
    , _MTY_
    , _MUO_
    , _MVO_
    , _MVI_
    , _MTWK_
    , _MTWG_
    , _MTWN_
    , _MWH_
    , _MWK_
    , _MBKR_
    , _MBHR_
    , _MMT_
    , _MUDI_
    , _MEER_
    , _MUP_
    , _LLN_
    , _MBBY_
    , _MWR_
    , _MAA_
    , _MUNM_
    , _MHY_
    , _MBD_
    , _MYW_
    , _MMM_
    , _MUI_
    , _MUR_
    , _MUEE_
    , _MRTL_
    , _NMB_
    , _NHS_
    , _NAA_
    , _NAMA_
    , _NRW_
    , _NOOG_
    , _NKBO_
    , _NEM_
    , _NPBR_
    , _NEN_
    , _NGI_
    , _NTT_
    , _NIM_
    , _NDI_
    , _NARL_
    , _NBB_
    , _NDY_
    , _NOME_
    , _NNDO_
    , _NOSA_
    , _NEQ_
    , _NOHD_
    , _NORT_
    , _NSTA_
    , _NBRR_
    , _NRWN_
    , _NUDG_
    , _NUA_
    , _NUN_
    , _NUPA_
    , _NCHU_
    , _OAT_
    , _OBC_
    , _OBSH_
    , _OBY_
    , _OLCO_
    , _OLSOD_
    , _ONPK_
    , _ORKS_
    , _ORF_
    , _OHB_
    , _OVL_
    , _OEN_
    , _OWS_
    , _OFD_
    , _PCVE_
    , _PFRM_
    , _PLW_
    , _PLU_
    , _SFG_
    , _PKR_
    , _PRKH_
    , _PRT_
    , _PAA_
    , _PECO_
    , _PDNE_
    , _PEAR_
    , _PCCK_
    , _PENH_
    , _PENT_
    , _PVS_
    , _PEG_
    , _PCKD_
    , _PCTY_
    , _PTI_
    , _PIB_
    , _PIL_
    , _PCA_
    , _PNP_
    , _PIC_
    , _PIG_
    , _PIPT_
    , _PING_
    , _PII_
    , _PWH_
    , _PTOM_
    , _PSS_
    , _PRP_
    , _PAL_
    , _PMA_
    , _POMP_
    , _PDV_
    , _PTD_
    , _PJUL_
    , _PMG_
    , _PNE_
    , _PNL_
    , _PIPS_
    , _PVCT_
    , _POWR_
    , _PWLC_
    , _PRES_
    , _PRS_
    , _PCB_
    , _PSP_
    , _PSF_
    , _PSTO_
    , _DNGR_
    , _FAW_
    , _PMPH_
    , _PBF_
    , _PWDA_
    , _PUTY_
    , _PYA_
    , _PYK_
    , _QE2_
    , _QAI_
    , _QBN_
    , _Q1_
    , _QLW_
    , _QNDI_
    , _RIL_
    , _RBY_
    , _RDRS_
    , _RADT_
    , _RAIS_
    , _RNN_
    , _RAPD_
    , _RTY_
    , _RKI_
    , _RCS_
    , _RDHI_
    , _RER_
    , _REDC_
    , _RDV_
    , _REDF_
    , _REDB_
    , _REDL_
    , _RENR_
    , _RESC_
    , _RCH_
    , _RIT_
    , _RMH_
    , _RIV_
    , _RVTN_
    , _ROTC_
    , _ROK_
    , _ROHM_
    , _RLY_
    , _RGS_
    , _RKWC_
    , _RSH_
    , _RVB_
    , _RSWD_
    , _RSEW_
    , _RRDM_
    , _RLR_
    , _RMT_
    , _RCB_
    , _RUIS_
    , _RYB_
    , _SADD_
    , _SSV_
    , _SAU_
    , _SALW_
    , _SSTO_
    , _SAND_
    , _SDP_
    , _STT_
    , _SJI_
    , _SRIN_
    , _SVR_
    , _SWTE_
    , _SWY_
    , _STC_
    , _STTE_
    , _SECF_
    , _SVAL_
    , _SLB_
    , _SDS_
    , _SHCR_
    , _SHI_
    , _SHL_
    , _SEL_
    , _SHOAL_
    , _SSG_
    , _SIXS_
    , _SKP_
    , _SLPT_
    , _SGK_
    , _SLP_
    , _SMIF_
    , _SYI_
    , _SOFA_
    , _SRP_
    , _SMD_
    , _SMN_
    , _SORL_
    , _SEC_
    , _SMIB_
    , _SPR_
    , _SPN_
    , _SDG_
    , _SBRR_
    , _SPT_
    , _STR_
    , _SPIT_
    , _SRR_
    , _SOI_
    , _SPMT_
    , _SBK_
    , _SGM_
    , _SHIS_
    , _SIS_
    , _SKI_
    , _SRY_
    , _SLL_
    , _SPS_
    , _STARF_
    , _SNP_
    , _STPK_
    , _SCRK_
    , _SBD_
    , _STOT_
    , _SYN_
    , _SFE_
    , _STRA_
    , _SLY_
    , _SRO_
    , _SUA_
    , _STUM_
    , _SUI_
    , _SUB_
    , _SUTR_
    , _SLMT_
    , _SUG_
    , _SGSV_
    , _SWLD_
    , _SUNZ_
    , _SBU_
    , _SWT_
    , _SBIT_
    , _SUPA_
    , _SUD_
    , _SHER_
    , _SUE_
    , _SBCH_
    , _SCTY_
    , _SCG_
    , _SYHD_
    , _SYP_
    , _TBL_
    , _TCH_
    , _TLAG_
    , _TLY_
    , _TRWL_
    , _TLK_
    , _TID_
    , _TGN_
    , _TUND_
    , _TAGO_
    , _TOWI_
    , _TRT_
    , _TAP_
    , _TAR_
    , _TLEE_
    , _TAS_
    , _CKO_
    , _TPL_
    , _TLC_
    , _TWT_
    , _THW_
    , _TCNR_
    , _THUM_
    , _TGU_
    , _TLMI_
    , _THK_
    , _PIN_
    , _RCK_
    , _APST_
    , _WHF_
    , _THB_
    , _THSM_
    , _TORN_
    , _TNP_
    , _TBRT_
    , _TING_
    , _TMPT_
    , _TOO_
    , _TLN_
    , _TOOU_
    , _TOGA_
    , _TOWA_
    , _TVM_
    , _TRIN_
    , _TRUO_
    , _TKER_
    , _TUK_
    , _TGC_
    , _TUMB_
    , _TTLE_
    , _TWRN_
    , _TOS_
    , _UKER_
    , _UDA_
    , _UNDW_
    , _UQLD_
    , _USW_
    , _URAN_
    , _URA_
    , _URC_
    , _URU_
    , _VTG_
    , _VELO_
    , _VICP_
    , _VOKH_
    , _VPH_
    , _WGL_
    , _WTC_
    , _WKT_
    , _WBH_
    , _WAN_
    , _WGR_
    , _WRO_
    , _WMB_
    , _WPE_
    , _WTBG_
    , _WDN_
    , _WAND_
    , _WAT_
    , _WAG_
    , _WRNA_
    , _WAD_
    , _WASL_
    , _WRD_
    , _WRR_
    , _WFM_
    , _WFL_
    , _WAYS_
    , _WWN_
    , _WCP_
    , _WELL_
    , _WLS_
    , _WELS_
    , _WBER_
    , _WBES_
    , _WEK_
    , _WSM_
    , _WEBS_
    , _WTG_
    , _WSN_
    , _WEP_
    , _WNFE_
    , _WES_
    , _WMR_
    , _WST_
    , _WTB_
    , _WHM_
    , _WTRK_
    , _WHW_
    , _WTS_
    , _WIKP_
    , _WHPL_
    , _WILE_
    , _WMS_
    , _WICK_
    , _WIE_
    , _WDU_
    , _WSFR_
    , _WTHB_
    , _WHDW_
    , _WBK_
    , _WOT_
    , _WOOS_
    , _WMP_
    , _WYPT_
    , _WGG_
    , _WOI_
    , _WNK_
    , _WAM_
    , _WRM_
    , _WUN_
    , _WUTUL_
    , _WMF_
    , _WYR_
    , _YMA_
    , _YBU_
    , _YKH_
    , _ALBO_
    , _YYN_
    , _ANDA_
    , _YNA_
    , _YGB_
    , _YYM_
    , _YBH_
    , _ASST_
    , _ORKT_
    , _YKS_
    , _ORNN_
    , _ULAB_
    , _URAR_
    , _ZIN_
    ]
