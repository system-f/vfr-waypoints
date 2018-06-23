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

data VFR_Waypoint =
  VFR_Waypoint
    String
    (Maybe String)
    String
    Double
    Double
  deriving (Eq, Ord, Show)

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
    Lens' a Double
  lat =
    vfr_waypoint . lat
  {-# INLINE lat #-}

  lon ::
    Lens' a Double
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
    (-23.541666666666668)
    133.55

_ABKL_ ::
  VFR_Waypoint
_ABKL_ =
  VFR_Waypoint
    "ABM KILTO"
    (Just "WA")
    "ABKL"
    (-17.738333333333333)
    122.735

_TVT_ ::
  VFR_Waypoint
_TVT_ =
  VFR_Waypoint
    "ABM TV TOWERS"
    (Just "QLD")
    "TVT"
    (-27.475)
    152.91666666666666

_ACE_ ::
  VFR_Waypoint
_ACE_ =
  VFR_Waypoint
    "ACADEMY"
    (Just "VIC")
    "ACE"
    (-37.89666666666667)
    145.18

_ANI_ ::
  VFR_Waypoint
_ANI_ =
  VFR_Waypoint
    "ACHERON ISLAND"
    (Just "QLD")
    "ANI"
    (-18.961666666666666)
    146.63666666666666

_ACLD_ ::
  VFR_Waypoint
_ACLD_ =
  VFR_Waypoint
    "ACLAND"
    (Just "QLD")
    "ACLD"
    (-27.305)
    151.68833333333333

_ACTY_ ::
  VFR_Waypoint
_ACTY_ =
  VFR_Waypoint
    "ADELAIDE CBD"
    (Just "SA")
    "ACTY"
    (-34.93333333333333)
    138.6

_ADB_ ::
  VFR_Waypoint
_ADB_ =
  VFR_Waypoint
    "ADELAIDE RIVER BRIDGE"
    (Just "NT")
    "ADB"
    (-12.658333333333333)
    131.33333333333334

_AOG_ ::
  VFR_Waypoint
_AOG_ =
  VFR_Waypoint
    "ADELONG"
    (Just "NSW")
    "AOG"
    (-35.30833333333333)
    148.06666666666666

_ADI_ ::
  VFR_Waypoint
_ADI_ =
  VFR_Waypoint
    "ADMIRALTY ISLAND"
    (Just "QLD")
    "ADI"
    (-16.983333333333334)
    145.775

_ADWD_ ::
  VFR_Waypoint
_ADWD_ =
  VFR_Waypoint
    "ADVENTURE WORLD"
    (Just "WA")
    "ADWD"
    (-32.095)
    115.81833333333333

_ALON_ ::
  VFR_Waypoint
_ALON_ =
  VFR_Waypoint
    "ALAWOONA"
    (Just "SA")
    "ALON"
    (-34.75)
    140.5

_APL_ ::
  VFR_Waypoint
_APL_ =
  VFR_Waypoint
    "ALBERT PARK LAKE"
    (Just "VIC")
    "APL"
    (-37.855)
    144.975

_AKW_ ::
  VFR_Waypoint
_AKW_ =
  VFR_Waypoint
    "ALKIMOS WRECK"
    (Just "WA")
    "AKW"
    (-31.608333333333334)
    115.65

_ALBA_ ::
  VFR_Waypoint
_ALBA_ =
  VFR_Waypoint
    "ALOOMBA"
    (Just "QLD")
    "ALBA"
    (-17.105)
    145.83333333333334

_ALOA_ ::
  VFR_Waypoint
_ALOA_ =
  VFR_Waypoint
    "ALTONA"
    (Just "VIC")
    "ALOA"
    (-37.86666666666667)
    144.85

_ALTS_ ::
  VFR_Waypoint
_ALTS_ =
  VFR_Waypoint
    "ALTONA SOUTH"
    (Just "VIC")
    "ALTS"
    (-37.87833333333333)
    144.81

_ANG_ ::
  VFR_Waypoint
_ANG_ =
  VFR_Waypoint
    "ANGLESEA"
    (Just "VIC")
    "ANG"
    (-38.416666666666664)
    144.18333333333334

_APM_ ::
  VFR_Waypoint
_APM_ =
  VFR_Waypoint
    "ANM PAPER MILL"
    (Just "NSW")
    "APM"
    (-36.0)
    146.98333333333332

_ANA_ ::
  VFR_Waypoint
_ANA_ =
  VFR_Waypoint
    "ANNA BAY"
    (Just "NSW")
    "ANA"
    (-32.78333333333333)
    152.08333333333334

_ANP_ ::
  VFR_Waypoint
_ANP_ =
  VFR_Waypoint
    "ANTILL PLAINS"
    (Just "QLD")
    "ANP"
    (-19.433333333333334)
    146.83333333333334

_APPN_ ::
  VFR_Waypoint
_APPN_ =
  VFR_Waypoint
    "APPIN"
    (Just "NSW")
    "APPN"
    (-34.2)
    150.78833333333333

_ARCD_ ::
  VFR_Waypoint
_ARCD_ =
  VFR_Waypoint
    "ARCADIA HS"
    (Just "QLD")
    "ARCD"
    (-20.866666666666667)
    138.06666666666666

_AEN_ ::
  VFR_Waypoint
_AEN_ =
  VFR_Waypoint
    "ARDENT"
    (Just "QLD")
    "AEN"
    (-26.778333333333332)
    152.57833333333335

_ARE_ ::
  VFR_Waypoint
_ARE_ =
  VFR_Waypoint
    "ARMADALE"
    (Just "WA")
    "ARE"
    (-32.14333333333333)
    116.01333333333334

_AWP_ ::
  VFR_Waypoint
_AWP_ =
  VFR_Waypoint
    "ARROWSMITH PT"
    (Just "NT")
    "AWP"
    (-13.25)
    136.45

_ASU_ ::
  VFR_Waypoint
_ASU_ =
  VFR_Waypoint
    "ARUNDEL SUBSTATION"
    (Just "NSW")
    "ASU"
    (-35.208333333333336)
    147.4

_ATN_ ::
  VFR_Waypoint
_ATN_ =
  VFR_Waypoint
    "ATHERTON"
    (Just "QLD")
    "ATN"
    (-17.258333333333333)
    145.50833333333333

_ATG_ ::
  VFR_Waypoint
_ATG_ =
  VFR_Waypoint
    "ATTUNGA"
    (Just "NSW")
    "ATG"
    (-30.933333333333334)
    150.83833333333334

_ATV_ ::
  VFR_Waypoint
_ATV_ =
  VFR_Waypoint
    "ATV10"
    (Just "VIC")
    "ATV"
    (-37.85333333333333)
    145.16833333333332

_ALEC_ ::
  VFR_Waypoint
_ALEC_ =
  VFR_Waypoint
    "AUSTRALIAN LIVESTOCK EQUINE CENT"
    (Just "NSW")
    "ALEC"
    (-31.135)
    150.92166666666665

_AVCA_ ::
  VFR_Waypoint
_AVCA_ =
  VFR_Waypoint
    "AVOCA"
    (Just "TAS")
    "AVCA"
    (-41.781666666666666)
    147.72

_BADA_ ::
  VFR_Waypoint
_BADA_ =
  VFR_Waypoint
    "BABINDA"
    (Just "QLD")
    "BADA"
    (-17.341666666666665)
    145.925

_BMP_ ::
  VFR_Waypoint
_BMP_ =
  VFR_Waypoint
    "BACCHUS MARSH TOWNSHIP"
    (Just "VIC")
    "BMP"
    (-37.675)
    144.43833333333333

_BKRL_ ::
  VFR_Waypoint
_BKRL_ =
  VFR_Waypoint
    "BAKER LAKE"
    (Just "WA")
    "BKRL"
    (-26.711666666666666)
    125.98

_BLHS_ ::
  VFR_Waypoint
_BLHS_ =
  VFR_Waypoint
    "BALD HILLS MAST"
    (Just "QLD")
    "BLHS"
    (-27.313333333333333)
    153.01666666666668

_BOA_ ::
  VFR_Waypoint
_BOA_ =
  VFR_Waypoint
    "BALMORAL"
    (Just "VIC")
    "BOA"
    (-37.25)
    141.83333333333334

_BANG_ ::
  VFR_Waypoint
_BANG_ =
  VFR_Waypoint
    "BANGALOW"
    (Just "NSW")
    "BANG"
    (-28.686666666666667)
    153.51666666666668

_BDT_ ::
  VFR_Waypoint
_BDT_ =
  VFR_Waypoint
    "BARANDUDA TOWERS"
    (Just "VIC")
    "BDT"
    (-36.25)
    146.85

_BARB_ ::
  VFR_Waypoint
_BARB_ =
  VFR_Waypoint
    "BARBAGALLO RACEWAY"
    (Just "WA")
    "BARB"
    (-31.666666666666668)
    115.78333333333333

_BHCP_ ::
  VFR_Waypoint
_BHCP_ =
  VFR_Waypoint
    "BARN HILL CARAVAN PARK"
    (Just "WA")
    "BHCP"
    (-18.368333333333332)
    122.04

_BSL_ ::
  VFR_Waypoint
_BSL_ =
  VFR_Waypoint
    "BARNES HILL"
    (Just "QLD")
    "BSL"
    (-27.2)
    152.1

_BRNJ_ ::
  VFR_Waypoint
_BRNJ_ =
  VFR_Waypoint
    "BARRENJOEY HEAD"
    (Just "NSW")
    "BRNJ"
    (-33.58)
    151.32666666666665

_BRR_ ::
  VFR_Waypoint
_BRR_ =
  VFR_Waypoint
    "BARRINGUN"
    (Just "NSW")
    "BRR"
    (-29.016666666666666)
    145.7

_BRGE_ ::
  VFR_Waypoint
_BRGE_ =
  VFR_Waypoint
    "BARRON GORGE"
    (Just "QLD")
    "BRGE"
    (-16.85)
    145.65

_BPN_ ::
  VFR_Waypoint
_BPN_ =
  VFR_Waypoint
    "BARWON PRISON"
    (Just "VIC")
    "BPN"
    (-37.983333333333334)
    144.35

_KALK_ ::
  VFR_Waypoint
_KALK_ =
  VFR_Waypoint
    "BASS STRAIT"
    (Just "TAS")
    "KALK"
    (-39.5)
    142.5

_BATB_ ::
  VFR_Waypoint
_BATB_ =
  VFR_Waypoint
    "BATEMANS BAY"
    (Just "NSW")
    "BATB"
    (-35.71666666666667)
    150.18333333333334

_BTI_ ::
  VFR_Waypoint
_BTI_ =
  VFR_Waypoint
    "BATHURST ISLAND"
    (Just "NT")
    "BTI"
    (-11.766666666666667)
    130.61666666666667

_BNE_ ::
  VFR_Waypoint
_BNE_ =
  VFR_Waypoint
    "BATMAN BRIDGE"
    (Just "TAS")
    "BNE"
    (-41.218333333333334)
    146.915

_BAW_ ::
  VFR_Waypoint
_BAW_ =
  VFR_Waypoint
    "BAYWEST"
    (Just "VIC")
    "BAW"
    (-38.0)
    144.92666666666668

_BCM_ ::
  VFR_Waypoint
_BCM_ =
  VFR_Waypoint
    "BEACHMERE"
    (Just "QLD")
    "BCM"
    (-27.116666666666667)
    153.05

_BIV_ ::
  VFR_Waypoint
_BIV_ =
  VFR_Waypoint
    "BECTIVE HS"
    (Just "NSW")
    "BIV"
    (-30.971666666666668)
    150.73833333333334

_BENT_ ::
  VFR_Waypoint
_BENT_ =
  VFR_Waypoint
    "BEECHMONT"
    (Just "QLD")
    "BENT"
    (-28.133333333333333)
    153.2

_BCH_ ::
  VFR_Waypoint
_BCH_ =
  VFR_Waypoint
    "BEECHWORTH"
    (Just "VIC")
    "BCH"
    (-36.358333333333334)
    146.68833333333333

_BLR_ ::
  VFR_Waypoint
_BLR_ =
  VFR_Waypoint
    "BELLBROOK"
    (Just "NSW")
    "BLR"
    (-30.821666666666665)
    152.51166666666666

_BLIG_ ::
  VFR_Waypoint
_BLIG_ =
  VFR_Waypoint
    "BELLINGEN"
    (Just "NSW")
    "BLIG"
    (-30.455)
    152.89666666666668

_BENV_ ::
  VFR_Waypoint
_BENV_ =
  VFR_Waypoint
    "BEN NEVIS"
    (Just "TAS")
    "BENV"
    (-41.41166666666667)
    147.64166666666668

_BND_ ::
  VFR_Waypoint
_BND_ =
  VFR_Waypoint
    "BENDEMEER"
    (Just "NSW")
    "BND"
    (-30.883333333333333)
    151.15

_BEE_ ::
  VFR_Waypoint
_BEE_ =
  VFR_Waypoint
    "BERILEE"
    (Just "NSW")
    "BEE"
    (-33.62)
    151.105

_BESI_ ::
  VFR_Waypoint
_BESI_ =
  VFR_Waypoint
    "BESSIE POINT"
    (Just "QLD")
    "BESI"
    (-16.903333333333332)
    145.81333333333333

_BVG_ ::
  VFR_Waypoint
_BVG_ =
  VFR_Waypoint
    "BEVERIDGE"
    (Just "VIC")
    "BVG"
    (-37.471666666666664)
    144.97166666666666

_BDTN_ ::
  VFR_Waypoint
_BDTN_ =
  VFR_Waypoint
    "BIDDESTON"
    (Just "QLD")
    "BDTN"
    (-27.558333333333334)
    151.71666666666667

_BIBA_ ::
  VFR_Waypoint
_BIBA_ =
  VFR_Waypoint
    "BINGIL BAY"
    (Just "QLD")
    "BIBA"
    (-17.828333333333333)
    146.10166666666666

_BDWD_ ::
  VFR_Waypoint
_BDWD_ =
  VFR_Waypoint
    "BIRDWOOD"
    (Just "SA")
    "BDWD"
    (-34.82333333333333)
    138.96

_BCT_ ::
  VFR_Waypoint
_BCT_ =
  VFR_Waypoint
    "BLACK MT"
    (Just "QLD")
    "BCT"
    (-21.07)
    149.09666666666666

_BKM_ ::
  VFR_Waypoint
_BKM_ =
  VFR_Waypoint
    "BLACK MT"
    (Just "ACT")
    "BKM"
    (-35.275)
    149.1

_BKIS_ ::
  VFR_Waypoint
_BKIS_ =
  VFR_Waypoint
    "BLACKSMITH ISLAND"
    (Just "QLD")
    "BKIS"
    (-20.633333333333333)
    149.06666666666666

_BLIC_ ::
  VFR_Waypoint
_BLIC_ =
  VFR_Waypoint
    "BLI BLI CASTLE"
    (Just "QLD")
    "BLIC"
    (-26.625)
    153.03333333333333

_BTM_ ::
  VFR_Waypoint
_BTM_ =
  VFR_Waypoint
    "BOAT HARBOUR"
    (Just "TAS")
    "BTM"
    (-40.95)
    145.63333333333333

_BOAT_ ::
  VFR_Waypoint
_BOAT_ =
  VFR_Waypoint
    "BOATYARD"
    (Just "WA")
    "BOAT"
    (-32.15)
    115.76666666666667

_BODD_ ::
  VFR_Waypoint
_BODD_ =
  VFR_Waypoint
    "BODDINGTON"
    (Just "WA")
    "BODD"
    (-32.8)
    116.46666666666667

_BGN_ ::
  VFR_Waypoint
_BGN_ =
  VFR_Waypoint
    "BOGANTUNGAN"
    (Just "QLD")
    "BGN"
    (-23.65)
    147.3

_BLTB_ ::
  VFR_Waypoint
_BLTB_ =
  VFR_Waypoint
    "BOLTE BRIDGE"
    (Just "VIC")
    "BLTB"
    (-37.82)
    144.93166666666667

_BSP_ ::
  VFR_Waypoint
_BSP_ =
  VFR_Waypoint
    "BOND SPRINGS"
    (Just "NT")
    "BSP"
    (-23.516666666666666)
    133.85

_BUVY_ ::
  VFR_Waypoint
_BUVY_ =
  VFR_Waypoint
    "BOND UNIVERSITY"
    (Just "QLD")
    "BUVY"
    (-28.076666666666668)
    153.41

_BNG_ ::
  VFR_Waypoint
_BNG_ =
  VFR_Waypoint
    "BONEGILLA"
    (Just "VIC")
    "BNG"
    (-36.145)
    147.01333333333332

_BONG_ ::
  VFR_Waypoint
_BONG_ =
  VFR_Waypoint
    "BONGAREE"
    (Just "QLD")
    "BONG"
    (-27.083333333333332)
    153.18333333333334

_BOON_ ::
  VFR_Waypoint
_BOON_ =
  VFR_Waypoint
    "BOONDALL ENTERTAINMENT CENTRE"
    (Just "QLD")
    "BOON"
    (-27.341666666666665)
    153.085

_BOAR_ ::
  VFR_Waypoint
_BOAR_ =
  VFR_Waypoint
    "BORUMBA RESV"
    (Just "QLD")
    "BOAR"
    (-26.508333333333333)
    152.58333333333334

_BBH_ ::
  VFR_Waypoint
_BBH_ =
  VFR_Waypoint
    "BOTANY BAY HEADS"
    (Just "NSW")
    "BBH"
    (-34.00833333333333)
    151.24166666666667

_BOWEN_ ::
  VFR_Waypoint
_BOWEN_ =
  VFR_Waypoint
    "BOWEN"
    (Just "QLD")
    "BOWEN"
    (-20.016666666666666)
    148.24166666666667

_BOWB_ ::
  VFR_Waypoint
_BOWB_ =
  VFR_Waypoint
    "BOWEN BRIDGE"
    (Just "TAS")
    "BOWB"
    (-42.81666666666667)
    147.3

_BWV_ ::
  VFR_Waypoint
_BWV_ =
  VFR_Waypoint
    "BOWENVILLE"
    (Just "QLD")
    "BWV"
    (-27.305)
    151.48833333333334

_BWMS_ ::
  VFR_Waypoint
_BWMS_ =
  VFR_Waypoint
    "BOWMANS"
    (Just "SA")
    "BWMS"
    (-34.15)
    138.26666666666668

_BOW_ ::
  VFR_Waypoint
_BOW_ =
  VFR_Waypoint
    "BOWNA"
    (Just "NSW")
    "BOW"
    (-35.95)
    147.11666666666667

_BWL_ ::
  VFR_Waypoint
_BWL_ =
  VFR_Waypoint
    "BOWRAL"
    (Just "NSW")
    "BWL"
    (-34.48)
    150.41833333333332

_BOV_ ::
  VFR_Waypoint
_BOV_ =
  VFR_Waypoint
    "BOWRAVILLE"
    (Just "NSW")
    "BOV"
    (-30.65)
    152.85

_BPI_ ::
  VFR_Waypoint
_BPI_ =
  VFR_Waypoint
    "BRAMPTON ISLAND"
    (Just "QLD")
    "BPI"
    (-20.805)
    149.26666666666668

_BAXT_ ::
  VFR_Waypoint
_BAXT_ =
  VFR_Waypoint
    "BRANXTON"
    (Just "NSW")
    "BAXT"
    (-32.666666666666664)
    151.35

_BZA_ ::
  VFR_Waypoint
_BZA_ =
  VFR_Waypoint
    "BREEZA"
    (Just "NSW")
    "BZA"
    (-31.25)
    150.46666666666667

_BBBG_ ::
  VFR_Waypoint
_BBBG_ =
  VFR_Waypoint
    "BRIBIE BRIDGE"
    (Just "QLD")
    "BBBG"
    (-27.073333333333334)
    153.14666666666668

_BBI_ ::
  VFR_Waypoint
_BBI_ =
  VFR_Waypoint
    "BRIBIE ISLAND"
    (Just "QLD")
    "BBI"
    (-27.0)
    153.14333333333335

_BTO_ ::
  VFR_Waypoint
_BTO_ =
  VFR_Waypoint
    "BRIGHTON"
    (Just "VIC")
    "BTO"
    (-37.91166666666667)
    144.98666666666668

_BTJ_ ::
  VFR_Waypoint
_BTJ_ =
  VFR_Waypoint
    "BRIGHTON JETTY"
    (Just "SA")
    "BTJ"
    (-35.016666666666666)
    138.51666666666668

_BRY_ ::
  VFR_Waypoint
_BRY_ =
  VFR_Waypoint
    "BRINGELLY"
    (Just "NSW")
    "BRY"
    (-33.94166666666667)
    150.72833333333332

_BRIN_ ::
  VFR_Waypoint
_BRIN_ =
  VFR_Waypoint
    "BRINSMEAD"
    (Just "QLD")
    "BRIN"
    (-16.908333333333335)
    145.70833333333334

_BCTY_ ::
  VFR_Waypoint
_BCTY_ =
  VFR_Waypoint
    "BRISBANE CBD"
    (Just "QLD")
    "BCTY"
    (-27.466666666666665)
    153.03333333333333

_OGABA_ ::
  VFR_Waypoint
_OGABA_ =
  VFR_Waypoint
    "BRISBANE CRICKET GROUND"
    (Just "QLD")
    "OGABA"
    (-27.48)
    153.06666666666666

_BDF_ ::
  VFR_Waypoint
_BDF_ =
  VFR_Waypoint
    "BROADFORD"
    (Just "VIC")
    "BDF"
    (-37.208333333333336)
    145.04166666666666

_BYN_ ::
  VFR_Waypoint
_BYN_ =
  VFR_Waypoint
    "BROOKLYN"
    (Just "VIC")
    "BYN"
    (-37.82833333333333)
    144.85833333333332

_BBG_ ::
  VFR_Waypoint
_BBG_ =
  VFR_Waypoint
    "BROOKLYN BRIDGE"
    (Just "NSW")
    "BBG"
    (-33.541666666666664)
    151.19666666666666

_BTON_ ::
  VFR_Waypoint
_BTON_ =
  VFR_Waypoint
    "BROOKTON"
    (Just "WA")
    "BTON"
    (-32.36666666666667)
    117.01666666666667

_BRI_ ::
  VFR_Waypoint
_BRI_ =
  VFR_Waypoint
    "BROUGHTON ISLAND"
    (Just "NSW")
    "BRI"
    (-32.608333333333334)
    152.30833333333334

_OBSTM_ ::
  VFR_Waypoint
_OBSTM_ =
  VFR_Waypoint
    "BRUCE STADIUM"
    (Just "ACT")
    "OBSTM"
    (-35.25)
    149.1

_BRH_ ::
  VFR_Waypoint
_BRH_ =
  VFR_Waypoint
    "BRUNSWICK HEADS"
    (Just "NSW")
    "BRH"
    (-28.541666666666668)
    153.55

_BUCP_ ::
  VFR_Waypoint
_BUCP_ =
  VFR_Waypoint
    "BUCHAN PT"
    (Just "QLD")
    "BUCP"
    (-16.736666666666668)
    145.66833333333332

_BUCN_ ::
  VFR_Waypoint
_BUCN_ =
  VFR_Waypoint
    "BUCHANAN HILLS"
    (Just "NT")
    "BUCN"
    (-18.9)
    131.08333333333334

_BKD_ ::
  VFR_Waypoint
_BKD_ =
  VFR_Waypoint
    "BUCKLAND"
    (Just "TAS")
    "BKD"
    (-42.611666666666665)
    147.71666666666667

_BPK_ ::
  VFR_Waypoint
_BPK_ =
  VFR_Waypoint
    "BUCKLAND PARK WEATHER RADAR"
    (Just "SA")
    "BPK"
    (-34.61666666666667)
    138.46833333333333

_BDH_ ::
  VFR_Waypoint
_BDH_ =
  VFR_Waypoint
    "BUNDAGEN HEAD"
    (Just "NSW")
    "BDH"
    (-30.433333333333334)
    153.075

_BEN_ ::
  VFR_Waypoint
_BEN_ =
  VFR_Waypoint
    "BUNGENDORE"
    (Just "NSW")
    "BEN"
    (-35.25833333333333)
    149.44666666666666

_BUG_ ::
  VFR_Waypoint
_BUG_ =
  VFR_Waypoint
    "BUNGIL BRIDGE"
    (Just "VIC")
    "BUG"
    (-36.05)
    147.35

_URG_ ::
  VFR_Waypoint
_URG_ =
  VFR_Waypoint
    "BURBONG"
    (Just "NSW")
    "URG"
    (-35.34)
    149.31

_BLGH_ ::
  VFR_Waypoint
_BLGH_ =
  VFR_Waypoint
    "BURLEIGH HEADS"
    (Just "QLD")
    "BLGH"
    (-28.091666666666665)
    153.45833333333334

_BUB_ ::
  VFR_Waypoint
_BUB_ =
  VFR_Waypoint
    "BURNS BEACH"
    (Just "WA")
    "BUB"
    (-31.72833333333333)
    115.71666666666667

_BURR_ ::
  VFR_Waypoint
_BURR_ =
  VFR_Waypoint
    "BURRA"
    (Just "SA")
    "BURR"
    (-33.68333333333333)
    138.93333333333334

_BUGA_ ::
  VFR_Waypoint
_BUGA_ =
  VFR_Waypoint
    "BURRAGA"
    (Just "NSW")
    "BUGA"
    (-33.88333333333333)
    149.56666666666666

_BJK_ ::
  VFR_Waypoint
_BJK_ =
  VFR_Waypoint
    "BURRINJUCK"
    (Just "NSW")
    "BJK"
    (-35.005)
    148.58333333333334

_BMK_ ::
  VFR_Waypoint
_BMK_ =
  VFR_Waypoint
    "BURRUMBUTTOCK"
    (Just "NSW")
    "BMK"
    (-35.833333333333336)
    146.8

_BYFD_ ::
  VFR_Waypoint
_BYFD_ =
  VFR_Waypoint
    "BYFORD"
    (Just "WA")
    "BYFD"
    (-32.21666666666667)
    116.03333333333333

_BYNO_ ::
  VFR_Waypoint
_BYNO_ =
  VFR_Waypoint
    "BYNOE HARBOUR"
    (Just "NT")
    "BYNO"
    (-12.75)
    130.68333333333334

_BYRK_ ::
  VFR_Waypoint
_BYRK_ =
  VFR_Waypoint
    "BYROCK"
    (Just "NSW")
    "BYRK"
    (-30.65)
    146.4

_BBAY_ ::
  VFR_Waypoint
_BBAY_ =
  VFR_Waypoint
    "BYRON BAY"
    (Just "NSW")
    "BBAY"
    (-28.65)
    153.61666666666667

_CBLT_ ::
  VFR_Waypoint
_CBLT_ =
  VFR_Waypoint
    "CABLEWAY TERMINAL"
    (Just "QLD")
    "CBLT"
    (-16.848333333333333)
    145.69166666666666

_CABO_ ::
  VFR_Waypoint
_CABO_ =
  VFR_Waypoint
    "CABOOLTURE"
    (Just "QLD")
    "CABO"
    (-27.088333333333335)
    152.95

_CALEN_ ::
  VFR_Waypoint
_CALEN_ =
  VFR_Waypoint
    "CALEN"
    (Just "QLD")
    "CALEN"
    (-20.9)
    148.77

_CRDZ_ ::
  VFR_Waypoint
_CRDZ_ =
  VFR_Waypoint
    "CALOUNDRA DROPZONE"
    (Just "QLD")
    "CRDZ"
    (-26.8)
    153.11

_CALT_ ::
  VFR_Waypoint
_CALT_ =
  VFR_Waypoint
    "CALTEX REFINERY"
    (Just "QLD")
    "CALT"
    (-27.415)
    153.15833333333333

_CBRA_ ::
  VFR_Waypoint
_CBRA_ =
  VFR_Waypoint
    "CAMBRAI"
    (Just "SA")
    "CBRA"
    (-34.65833333333333)
    139.28

_CPA_ ::
  VFR_Waypoint
_CPA_ =
  VFR_Waypoint
    "CAMPANIA"
    (Just "TAS")
    "CPA"
    (-42.666666666666664)
    147.42166666666665

_CLLN_ ::
  VFR_Waypoint
_CLLN_ =
  VFR_Waypoint
    "CAMPBELLTOWN"
    (Just "TAS")
    "CLLN"
    (-41.93333333333333)
    147.49166666666667

_CAMB_ ::
  VFR_Waypoint
_CAMB_ =
  VFR_Waypoint
    "CAMPBELLTOWN UNIVERSITY"
    (Just "NSW")
    "CAMB"
    (-34.071666666666665)
    150.78333333333333

_RCSE_ ::
  VFR_Waypoint
_RCSE_ =
  VFR_Waypoint
    "CANBERRA RACECOURSE"
    (Just "ACT")
    "RCSE"
    (-35.23833333333334)
    149.13666666666666

_CNB_ ::
  VFR_Waypoint
_CNB_ =
  VFR_Waypoint
    "CANNING BRIDGE"
    (Just "WA")
    "CNB"
    (-32.01)
    115.85

_CDM_ ::
  VFR_Waypoint
_CDM_ =
  VFR_Waypoint
    "CANNING DAM"
    (Just "WA")
    "CDM"
    (-32.155)
    116.125

_CAV_ ::
  VFR_Waypoint
_CAV_ =
  VFR_Waypoint
    "CANNONVALE"
    (Just "QLD")
    "CAV"
    (-20.278333333333332)
    148.69166666666666

_CBY_ ::
  VFR_Waypoint
_CBY_ =
  VFR_Waypoint
    "CANTERBURY RACECOURSE"
    (Just "NSW")
    "CBY"
    (-33.90833333333333)
    151.11166666666668

_CAPS_ ::
  VFR_Waypoint
_CAPS_ =
  VFR_Waypoint
    "CAPE BANKS"
    (Just "NSW")
    "CAPS"
    (-33.998333333333335)
    151.245

_CCL_ ::
  VFR_Waypoint
_CCL_ =
  VFR_Waypoint
    "CAPE CLEVELAND"
    (Just "QLD")
    "CCL"
    (-19.183333333333334)
    147.01333333333332

_CPY_ ::
  VFR_Waypoint
_CPY_ =
  VFR_Waypoint
    "CAPE CONWAY"
    (Just "QLD")
    "CPY"
    (-20.536666666666665)
    148.92833333333334

_CGR_ ::
  VFR_Waypoint
_CGR_ =
  VFR_Waypoint
    "CAPE GAMBIER"
    (Just "NT")
    "CGR"
    (-11.938333333333333)
    130.96666666666667

_CGF_ ::
  VFR_Waypoint
_CGF_ =
  VFR_Waypoint
    "CAPE GRAFTON"
    (Just "QLD")
    "CGF"
    (-16.863333333333333)
    145.91666666666666

_CPH_ ::
  VFR_Waypoint
_CPH_ =
  VFR_Waypoint
    "CAPE HILLSBOROUGH"
    (Just "QLD")
    "CPH"
    (-20.905)
    149.045

_CAJE_ ::
  VFR_Waypoint
_CAJE_ =
  VFR_Waypoint
    "CAPE JERVIS"
    (Just "SA")
    "CAJE"
    (-35.60666666666667)
    138.09166666666667

_CAKE_ ::
  VFR_Waypoint
_CAKE_ =
  VFR_Waypoint
    "CAPE KEITH"
    (Just "NT")
    "CAKE"
    (-11.616666666666667)
    131.46666666666667

_CMB_ ::
  VFR_Waypoint
_CMB_ =
  VFR_Waypoint
    "CAPE LAMBERT"
    (Just "WA")
    "CMB"
    (-20.593333333333334)
    117.18333333333334

_CPMN_ ::
  VFR_Waypoint
_CPMN_ =
  VFR_Waypoint
    "CAPE MORETON"
    (Just "QLD")
    "CPMN"
    (-27.033333333333335)
    153.46666666666667

_CPLD_ ::
  VFR_Waypoint
_CPLD_ =
  VFR_Waypoint
    "CAPE PORTLAND"
    (Just "TAS")
    "CPLD"
    (-40.75)
    147.95

_CPHE_ ::
  VFR_Waypoint
_CPHE_ =
  VFR_Waypoint
    "CAPE RICHE"
    (Just "WA")
    "CPHE"
    (-34.6)
    118.76666666666667

_CAPT_ ::
  VFR_Waypoint
_CAPT_ =
  VFR_Waypoint
    "CAPTAINS FLAT"
    (Just "NSW")
    "CAPT"
    (-35.59166666666667)
    149.445

_CAU_ ::
  VFR_Waypoint
_CAU_ =
  VFR_Waypoint
    "CARAMUT"
    (Just "VIC")
    "CAU"
    (-37.96666666666667)
    142.51666666666668

_CARE_ ::
  VFR_Waypoint
_CARE_ =
  VFR_Waypoint
    "CARDINIA RESV"
    (Just "VIC")
    "CARE"
    (-37.958333333333336)
    145.41666666666666

_CDNA_ ::
  VFR_Waypoint
_CDNA_ =
  VFR_Waypoint
    "CARDONA"
    (Just "QLD")
    "CDNA"
    (-23.333333333333332)
    149.0

_CARIN_ ::
  VFR_Waypoint
_CARIN_ =
  VFR_Waypoint
    "CARINDALE SHOPPING CENTRE"
    (Just "QLD")
    "CARIN"
    (-27.5)
    153.10166666666666

_CIK_ ::
  VFR_Waypoint
_CIK_ =
  VFR_Waypoint
    "CARRICK"
    (Just "TAS")
    "CIK"
    (-41.53333333333333)
    147.0

_CARR_ ::
  VFR_Waypoint
_CARR_ =
  VFR_Waypoint
    "CARRUM"
    (Just "VIC")
    "CARR"
    (-38.075)
    145.12

_CLJ_ ::
  VFR_Waypoint
_CLJ_ =
  VFR_Waypoint
    "CASTLE JUNCTION"
    (Just "TAS")
    "CLJ"
    (-41.505)
    147.48333333333332

_CTT_ ::
  VFR_Waypoint
_CTT_ =
  VFR_Waypoint
    "CASTLE PT"
    (Just "NT")
    "CTT"
    (-12.35)
    131.275

_CRPT_ ::
  VFR_Waypoint
_CRPT_ =
  VFR_Waypoint
    "CASTLEREAGH POINT"
    (Just "QLD")
    "CRPT"
    (-27.191666666666666)
    153.11166666666668

_CCP_ ::
  VFR_Waypoint
_CCP_ =
  VFR_Waypoint
    "CECIL PLAINS"
    (Just "QLD")
    "CCP"
    (-27.533333333333335)
    151.18333333333334

_CAO_ ::
  VFR_Waypoint
_CAO_ =
  VFR_Waypoint
    "CEDUNA OBSERVATORY"
    (Just "SA")
    "CAO"
    (-31.866666666666667)
    133.8

_CBRG_ ::
  VFR_Waypoint
_CBRG_ =
  VFR_Waypoint
    "CENTENARY BRIDGE"
    (Just "QLD")
    "CBRG"
    (-27.528333333333332)
    152.94666666666666

_CERB_ ::
  VFR_Waypoint
_CERB_ =
  VFR_Waypoint
    "CERBERUS"
    (Just "VIC")
    "CERB"
    (-37.96666666666667)
    145.0

_CYM_ ::
  VFR_Waypoint
_CYM_ =
  VFR_Waypoint
    "CHAFFEY DAM"
    (Just "NSW")
    "CYM"
    (-31.341666666666665)
    151.13333333333333

_CHI_ ::
  VFR_Waypoint
_CHI_ =
  VFR_Waypoint
    "CHANNEL ISLAND"
    (Just "NT")
    "CHI"
    (-12.55)
    130.86666666666667

_CHAP_ ::
  VFR_Waypoint
_CHAP_ =
  VFR_Waypoint
    "CHARLES PT"
    (Just "NT")
    "CHAP"
    (-12.383333333333333)
    130.61833333333334

_CHAT_ ::
  VFR_Waypoint
_CHAT_ =
  VFR_Waypoint
    "CHATSWOOD CBD"
    (Just "NSW")
    "CHAT"
    (-33.79666666666667)
    151.185

_CHN_ ::
  VFR_Waypoint
_CHN_ =
  VFR_Waypoint
    "CHILTERN"
    (Just "VIC")
    "CHN"
    (-36.14666666666667)
    146.60666666666665

_CIB_ ::
  VFR_Waypoint
_CIB_ =
  VFR_Waypoint
    "CHIPBOARD FACTORY"
    (Just "NSW")
    "CIB"
    (-35.071666666666665)
    147.405

_COY_ ::
  VFR_Waypoint
_COY_ =
  VFR_Waypoint
    "CHOCOLATE FACTORY"
    (Just "TAS")
    "COY"
    (-42.8)
    147.26666666666668

_CNTH_ ::
  VFR_Waypoint
_CNTH_ =
  VFR_Waypoint
    "CHOPPERS NORTH"
    (Just "NSW")
    "CNTH"
    (-33.88166666666667)
    151.025

_CSTH_ ::
  VFR_Waypoint
_CSTH_ =
  VFR_Waypoint
    "CHOPPERS SOUTH"
    (Just "NSW")
    "CSTH"
    (-33.955)
    150.965

_CWST_ ::
  VFR_Waypoint
_CWST_ =
  VFR_Waypoint
    "CHOPPERS WEST"
    (Just "NSW")
    "CWST"
    (-33.873333333333335)
    151.00333333333333

_CIH_ ::
  VFR_Waypoint
_CIH_ =
  VFR_Waypoint
    "CID HARBOUR"
    (Just "QLD")
    "CIH"
    (-20.25)
    148.93333333333334

_CYB_ ::
  VFR_Waypoint
_CYB_ =
  VFR_Waypoint
    "CITY BEACH"
    (Just "WA")
    "CYB"
    (-31.941666666666666)
    115.75

_CEN_ ::
  VFR_Waypoint
_CEN_ =
  VFR_Waypoint
    "CLEVEDON"
    (Just "QLD")
    "CEN"
    (-19.39666666666667)
    147.02166666666668

_CVD_ ::
  VFR_Waypoint
_CVD_ =
  VFR_Waypoint
    "CLEVELAND"
    (Just "QLD")
    "CVD"
    (-27.52)
    153.28333333333333

_CFI_ ::
  VFR_Waypoint
_CFI_ =
  VFR_Waypoint
    "CLIFFY ISLAND"
    (Just "VIC")
    "CFI"
    (-38.95166666666667)
    146.70166666666665

_CLS_ ::
  VFR_Waypoint
_CLS_ =
  VFR_Waypoint
    "CLIFTON SPRINGS"
    (Just "VIC")
    "CLS"
    (-38.15)
    144.56666666666666

_CGH_ ::
  VFR_Waypoint
_CGH_ =
  VFR_Waypoint
    "CLONAGH STN"
    (Just "QLD")
    "CGH"
    (-20.133333333333333)
    140.68333333333334

_CLOY_ ::
  VFR_Waypoint
_CLOY_ =
  VFR_Waypoint
    "CLONEYS CREEK"
    (Just "QLD")
    "CLOY"
    (-20.216666666666665)
    142.6

_COBA_ ::
  VFR_Waypoint
_COBA_ =
  VFR_Waypoint
    "COBAKI"
    (Just "NSW")
    "COBA"
    (-28.183333333333334)
    153.48833333333334

_CKT_ ::
  VFR_Waypoint
_CKT_ =
  VFR_Waypoint
    "COCKATOO"
    (Just "VIC")
    "CKT"
    (-37.93833333333333)
    145.495

_CBI_ ::
  VFR_Waypoint
_CBI_ =
  VFR_Waypoint
    "COLBINABBIN"
    (Just "VIC")
    "CBI"
    (-36.583333333333336)
    144.8

_CGE_ ::
  VFR_Waypoint
_CGE_ =
  VFR_Waypoint
    "COLLINGULLIE"
    (Just "NSW")
    "CGE"
    (-35.08833333333333)
    147.12166666666667

_CONG_ ::
  VFR_Waypoint
_CONG_ =
  VFR_Waypoint
    "COMERONG ISLAND"
    (Just "NSW")
    "CONG"
    (-34.88333333333333)
    150.73333333333332

_CJN_ ::
  VFR_Waypoint
_CJN_ =
  VFR_Waypoint
    "CONARA JUNCTION"
    (Just "TAS")
    "CJN"
    (-41.833333333333336)
    147.43333333333334

_CGM_ ::
  VFR_Waypoint
_CGM_ =
  VFR_Waypoint
    "CONDONG MILL"
    (Just "NSW")
    "CGM"
    (-28.316666666666666)
    153.43333333333334

_CBYC_ ::
  VFR_Waypoint
_CBYC_ =
  VFR_Waypoint
    "COOBY CREEK RESV"
    (Just "QLD")
    "CBYC"
    (-27.386666666666667)
    151.93833333333333

_CIS_ ::
  VFR_Waypoint
_CIS_ =
  VFR_Waypoint
    "COOK ISLAND"
    (Just "NSW")
    "CIS"
    (-28.196666666666665)
    153.57833333333335

_CLMN_ ::
  VFR_Waypoint
_CLMN_ =
  VFR_Waypoint
    "COOLAMON"
    (Just "NSW")
    "CLMN"
    (-34.81666666666667)
    147.2

_CMDR_ ::
  VFR_Waypoint
_CMDR_ =
  VFR_Waypoint
    "COOLMUNDA RESV"
    (Just "QLD")
    "CMDR"
    (-28.45)
    151.23333333333332

_COOL_ ::
  VFR_Waypoint
_COOL_ =
  VFR_Waypoint
    "COOLUM HI-RISE"
    (Just "QLD")
    "COOL"
    (-26.528333333333332)
    153.08666666666667

_CORO_ ::
  VFR_Waypoint
_CORO_ =
  VFR_Waypoint
    "COOROY"
    (Just "QLD")
    "CORO"
    (-26.416666666666668)
    152.90833333333333

_CPL_ ::
  VFR_Waypoint
_CPL_ =
  VFR_Waypoint
    "COPPERLODE DAM"
    (Just "QLD")
    "CPL"
    (-16.986666666666668)
    145.67166666666665

_CPNG_ ::
  VFR_Waypoint
_CPNG_ =
  VFR_Waypoint
    "COPPINS CROSSING"
    (Just "ACT")
    "CPNG"
    (-35.288333333333334)
    149.04333333333332

_CVR_ ::
  VFR_Waypoint
_CVR_ =
  VFR_Waypoint
    "CORIN RESV"
    (Just "ACT")
    "CVR"
    (-35.541666666666664)
    148.83333333333334

_COSS_ ::
  VFR_Waypoint
_COSS_ =
  VFR_Waypoint
    "COSSACK"
    (Just "WA")
    "COSS"
    (-20.676666666666666)
    117.19

_CTE_ ::
  VFR_Waypoint
_CTE_ =
  VFR_Waypoint
    "COTTESLOE"
    (Just "WA")
    "CTE"
    (-31.991666666666667)
    115.75

_COWI_ ::
  VFR_Waypoint
_COWI_ =
  VFR_Waypoint
    "COW ISLAND"
    (Just "QLD")
    "COWI"
    (-20.423333333333332)
    148.845

_COWR_ ::
  VFR_Waypoint
_COWR_ =
  VFR_Waypoint
    "COWWARR"
    (Just "VIC")
    "COWR"
    (-38.015)
    146.69333333333333

_CML_ ::
  VFR_Waypoint
_CML_ =
  VFR_Waypoint
    "CRADLE MOUNTAIN LODGE"
    (Just "TAS")
    "CML"
    (-41.595)
    145.92833333333334

_CBV_ ::
  VFR_Waypoint
_CBV_ =
  VFR_Waypoint
    "CRAIGBOURNE RESV"
    (Just "TAS")
    "CBV"
    (-42.55)
    147.41666666666666

_CGB_ ::
  VFR_Waypoint
_CGB_ =
  VFR_Waypoint
    "CRAIGIEBURN OVERPASS"
    (Just "VIC")
    "CGB"
    (-37.60333333333333)
    144.93833333333333

_CRAY_ ::
  VFR_Waypoint
_CRAY_ =
  VFR_Waypoint
    "CRAYFISH"
    (Just "SA")
    "CRAY"
    (-38.583333333333336)
    139.75

_CREM_ ::
  VFR_Waypoint
_CREM_ =
  VFR_Waypoint
    "CREMORNE"
    (Just "TAS")
    "CREM"
    (-42.958333333333336)
    147.53333333333333

_CES_ ::
  VFR_Waypoint
_CES_ =
  VFR_Waypoint
    "CRESSY"
    (Just "TAS")
    "CES"
    (-41.69166666666667)
    147.08333333333334

_CWK_ ::
  VFR_Waypoint
_CWK_ =
  VFR_Waypoint
    "CRESWICK"
    (Just "VIC")
    "CWK"
    (-37.43333333333333)
    143.9

_CUL_ ::
  VFR_Waypoint
_CUL_ =
  VFR_Waypoint
    "CRONULLA"
    (Just "NSW")
    "CUL"
    (-34.06166666666667)
    151.15333333333334

_CRPC_ ::
  VFR_Waypoint
_CRPC_ =
  VFR_Waypoint
    "CROPPA CREEK"
    (Just "NSW")
    "CRPC"
    (-29.133333333333333)
    150.3

_CNT_ ::
  VFR_Waypoint
_CNT_ =
  VFR_Waypoint
    "CROWS NEST"
    (Just "QLD")
    "CNT"
    (-27.27)
    152.055

_CGD_ ::
  VFR_Waypoint
_CGD_ =
  VFR_Waypoint
    "CUDGEN HEADLAND"
    (Just "NSW")
    "CGD"
    (-28.265)
    153.585

_CCN_ ::
  VFR_Waypoint
_CCN_ =
  VFR_Waypoint
    "CULCAIRN"
    (Just "NSW")
    "CCN"
    (-35.666666666666664)
    147.03833333333333

_DAIN_ ::
  VFR_Waypoint
_DAIN_ =
  VFR_Waypoint
    "DAINTREE"
    (Just "QLD")
    "DAIN"
    (-16.25)
    145.31666666666666

_DLMO_ ::
  VFR_Waypoint
_DLMO_ =
  VFR_Waypoint
    "DALMORE DOWNS"
    (Just "NT")
    "DLMO"
    (-19.775)
    136.00166666666667

_DRY_ ::
  VFR_Waypoint
_DRY_ =
  VFR_Waypoint
    "DALRYE"
    (Just "QLD")
    "DRY"
    (-20.166666666666668)
    149.06666666666666

_DMW_ ::
  VFR_Waypoint
_DMW_ =
  VFR_Waypoint
    "DAM WALL"
    (Just "SA")
    "DMW"
    (-34.75833333333333)
    138.72166666666666

_DARL_ ::
  VFR_Waypoint
_DARL_ =
  VFR_Waypoint
    "DARLIMURA"
    (Just "VIC")
    "DARL"
    (-38.35333333333333)
    146.215

_DHH_ ::
  VFR_Waypoint
_DHH_ =
  VFR_Waypoint
    "DARLING HARBOUR"
    (Just "NSW")
    "DHH"
    (-33.858333333333334)
    151.2

_DND_ ::
  VFR_Waypoint
_DND_ =
  VFR_Waypoint
    "DARWIN RIVER DAM"
    (Just "NT")
    "DND"
    (-12.825)
    130.96666666666667

_DBO_ ::
  VFR_Waypoint
_DBO_ =
  VFR_Waypoint
    "DAYBORO"
    (Just "QLD")
    "DBO"
    (-27.2)
    152.82166666666666

_DFD_ ::
  VFR_Waypoint
_DFD_ =
  VFR_Waypoint
    "DAYLESFORD"
    (Just "VIC")
    "DFD"
    (-37.35)
    144.15

_DGY_ ::
  VFR_Waypoint
_DGY_ =
  VFR_Waypoint
    "DE GREY HS"
    (Just "WA")
    "DGY"
    (-20.175)
    119.17

_DSS_ ::
  VFR_Waypoint
_DSS_ =
  VFR_Waypoint
    "DEDERANG SUBSTATION"
    (Just "VIC")
    "DSS"
    (-36.45333333333333)
    146.99

_DPW_ ::
  VFR_Waypoint
_DPW_ =
  VFR_Waypoint
    "DEEP WELL"
    (Just "NT")
    "DPW"
    (-24.358333333333334)
    134.05

_DEL_ ::
  VFR_Waypoint
_DEL_ =
  VFR_Waypoint
    "DELORAINE"
    (Just "TAS")
    "DEL"
    (-41.53333333333333)
    146.66666666666666

_DNP_ ::
  VFR_Waypoint
_DNP_ =
  VFR_Waypoint
    "DENHAM PASSAGE"
    (Just "QLD")
    "DNP"
    (-11.333333333333334)
    143.33333333333334

_DWB_ ::
  VFR_Waypoint
_DWB_ =
  VFR_Waypoint
    "DERWENT BRIDGE"
    (Just "TAS")
    "DWB"
    (-42.13333333333333)
    146.23333333333332

_DVM_ ::
  VFR_Waypoint
_DVM_ =
  VFR_Waypoint
    "DEVILS MARBLES"
    (Just "NT")
    "DVM"
    (-20.533333333333335)
    134.25

_DIBE_ ::
  VFR_Waypoint
_DIBE_ =
  VFR_Waypoint
    "DICKY BEACH"
    (Just "QLD")
    "DIBE"
    (-26.781666666666666)
    153.13833333333332

_DODI_ ::
  VFR_Waypoint
_DODI_ =
  VFR_Waypoint
    "DODDS ISLAND"
    (Just "NSW")
    "DODI"
    (-28.248333333333335)
    153.53333333333333

_DLPT_ ::
  VFR_Waypoint
_DLPT_ =
  VFR_Waypoint
    "DOLLS POINT"
    (Just "NSW")
    "DLPT"
    (-33.995)
    151.14833333333334

_DSN_ ::
  VFR_Waypoint
_DSN_ =
  VFR_Waypoint
    "DONCASTER SHOPPINGTOWN"
    (Just "VIC")
    "DSN"
    (-37.78333333333333)
    145.125

_DOP_ ::
  VFR_Waypoint
_DOP_ =
  VFR_Waypoint
    "DONNINGTON AIRPARK"
    (Just "QLD")
    "DOP"
    (-19.601666666666667)
    146.84166666666667

_DOOM_ ::
  VFR_Waypoint
_DOOM_ =
  VFR_Waypoint
    "DOOMBEN RACECOURSE"
    (Just "QLD")
    "DOOM"
    (-27.426666666666666)
    153.07

_DRO_ ::
  VFR_Waypoint
_DRO_ =
  VFR_Waypoint
    "DORRIGO"
    (Just "NSW")
    "DRO"
    (-30.341666666666665)
    152.71333333333334

_DCIS_ ::
  VFR_Waypoint
_DCIS_ =
  VFR_Waypoint
    "DOUBLE CONE ISLAND"
    (Just "QLD")
    "DCIS"
    (-20.1)
    148.71666666666667

_DOU_ ::
  VFR_Waypoint
_DOU_ =
  VFR_Waypoint
    "DOUBLE ISLAND"
    (Just "QLD")
    "DOU"
    (-16.725)
    145.68333333333334

_DLP_ ::
  VFR_Waypoint
_DLP_ =
  VFR_Waypoint
    "DOUBLE ISLAND PT"
    (Just "QLD")
    "DLP"
    (-25.916666666666668)
    153.18333333333334

_DBPT_ ::
  VFR_Waypoint
_DBPT_ =
  VFR_Waypoint
    "DOUBLE PT"
    (Just "QLD")
    "DBPT"
    (-11.866666666666667)
    142.9

_DRLD_ ::
  VFR_Waypoint
_DRLD_ =
  VFR_Waypoint
    "DREAMWORLD"
    (Just "QLD")
    "DRLD"
    (-27.865)
    153.31666666666666

_DRM_ ::
  VFR_Waypoint
_DRM_ =
  VFR_Waypoint
    "DROMANA"
    (Just "VIC")
    "DRM"
    (-38.333333333333336)
    144.96666666666667

_DRP_ ::
  VFR_Waypoint
_DRP_ =
  VFR_Waypoint
    "DROUGHTY PT"
    (Just "TAS")
    "DRP"
    (-42.93333333333333)
    147.41666666666666

_DRN_ ::
  VFR_Waypoint
_DRN_ =
  VFR_Waypoint
    "DROUIN"
    (Just "VIC")
    "DRN"
    (-38.13333333333333)
    145.85

_DCRK_ ::
  VFR_Waypoint
_DCRK_ =
  VFR_Waypoint
    "DRY CREEK"
    (Just "SA")
    "DCRK"
    (-34.833333333333336)
    138.58333333333334

_DAG_ ::
  VFR_Waypoint
_DAG_ =
  VFR_Waypoint
    "DUARINGA"
    (Just "QLD")
    "DAG"
    (-23.72)
    149.66833333333332

_DUB_ ::
  VFR_Waypoint
_DUB_ =
  VFR_Waypoint
    "DUBLIN"
    (Just "SA")
    "DUB"
    (-34.455)
    138.35

_DLY_ ::
  VFR_Waypoint
_DLY_ =
  VFR_Waypoint
    "DUNALLEY"
    (Just "TAS")
    "DLY"
    (-42.891666666666666)
    147.805

_DGN_ ::
  VFR_Waypoint
_DGN_ =
  VFR_Waypoint
    "DUNGOWAN"
    (Just "NSW")
    "DGN"
    (-31.216666666666665)
    151.11666666666667

_DUWN_ ::
  VFR_Waypoint
_DUWN_ =
  VFR_Waypoint
    "DUNGOWAN DAM"
    (Just "NSW")
    "DUWN"
    (-31.4)
    151.35

_DUA_ ::
  VFR_Waypoint
_DUA_ =
  VFR_Waypoint
    "DURI GAP"
    (Just "NSW")
    "DUA"
    (-31.2)
    150.7

_DMT_ ::
  VFR_Waypoint
_DMT_ =
  VFR_Waypoint
    "DURI MT"
    (Just "NSW")
    "DMT"
    (-31.205)
    150.73

_DONG_ ::
  VFR_Waypoint
_DONG_ =
  VFR_Waypoint
    "DURONG"
    (Just "QLD")
    "DONG"
    (-26.4)
    151.25

_DTON_ ::
  VFR_Waypoint
_DTON_ =
  VFR_Waypoint
    "DUTTON"
    (Just "SA")
    "DTON"
    (-34.36666666666667)
    139.13333333333333

_EAN_ ::
  VFR_Waypoint
_EAN_ =
  VFR_Waypoint
    "EAGLE HAWK NECK"
    (Just "TAS")
    "EAN"
    (-43.016666666666666)
    147.9

_EARH_ ::
  VFR_Waypoint
_EARH_ =
  VFR_Waypoint
    "EARL HILL"
    (Just "QLD")
    "EARH"
    (-16.8)
    145.7

_EARV_ ::
  VFR_Waypoint
_EARV_ =
  VFR_Waypoint
    "EARLVILLE"
    (Just "QLD")
    "EARV"
    (-16.955)
    145.73833333333334

_EAM_ ::
  VFR_Waypoint
_EAM_ =
  VFR_Waypoint
    "EAST ARM"
    (Just "NT")
    "EAM"
    (-12.516666666666667)
    130.93333333333334

_EGT_ ::
  VFR_Waypoint
_EGT_ =
  VFR_Waypoint
    "EAST GRETA"
    (Just "NSW")
    "EGT"
    (-32.73833333333333)
    151.53833333333333

_ETP_ ::
  VFR_Waypoint
_ETP_ =
  VFR_Waypoint
    "EAST PT"
    (Just "NT")
    "ETP"
    (-12.408333333333333)
    130.81666666666666

_EDP_ ::
  VFR_Waypoint
_EDP_ =
  VFR_Waypoint
    "EDDYSTONE PT"
    (Just "TAS")
    "EDP"
    (-40.998333333333335)
    148.34833333333333

_EDT_ ::
  VFR_Waypoint
_EDT_ =
  VFR_Waypoint
    "EDMONTON"
    (Just "QLD")
    "EDT"
    (-17.025)
    145.73

_ELDO_ ::
  VFR_Waypoint
_ELDO_ =
  VFR_Waypoint
    "ELDORADO"
    (Just "VIC")
    "ELDO"
    (-36.31166666666667)
    146.52166666666668

_ERB_ ::
  VFR_Waypoint
_ERB_ =
  VFR_Waypoint
    "ELIZABETH RIVER BRIDGE"
    (Just "NT")
    "ERB"
    (-12.543333333333333)
    130.975

_EMY_ ::
  VFR_Waypoint
_EMY_ =
  VFR_Waypoint
    "EMILY GAP"
    (Just "NT")
    "EMY"
    (-23.74)
    133.94166666666666

_EDOR_ ::
  VFR_Waypoint
_EDOR_ =
  VFR_Waypoint
    "ENDEAVOUR REEF"
    (Just "QLD")
    "EDOR"
    (-15.783333333333333)
    145.56666666666666

_EPPG_ ::
  VFR_Waypoint
_EPPG_ =
  VFR_Waypoint
    "EPPING"
    (Just "VIC")
    "EPPG"
    (-37.64333333333333)
    145.025

_ERSK_ ::
  VFR_Waypoint
_ERSK_ =
  VFR_Waypoint
    "ERSKINEVILLE OVAL"
    (Just "NSW")
    "ERSK"
    (-33.901666666666664)
    151.19

_ETON_ ::
  VFR_Waypoint
_ETON_ =
  VFR_Waypoint
    "ETON"
    (Just "QLD")
    "ETON"
    (-21.266666666666666)
    148.97166666666666

_EMI_ ::
  VFR_Waypoint
_EMI_ =
  VFR_Waypoint
    "EUMUNDI"
    (Just "QLD")
    "EMI"
    (-26.48)
    152.95333333333335

_EWI_ ::
  VFR_Waypoint
_EWI_ =
  VFR_Waypoint
    "EWANINGA"
    (Just "NT")
    "EWI"
    (-23.983333333333334)
    133.93333333333334

_EMD_ ::
  VFR_Waypoint
_EMD_ =
  VFR_Waypoint
    "EWEN MADDOCK DAM"
    (Just "QLD")
    "EMD"
    (-26.778333333333332)
    153.00833333333333

_FCP_ ::
  VFR_Waypoint
_FCP_ =
  VFR_Waypoint
    "FALSE CAPE"
    (Just "QLD")
    "FCP"
    (-16.875)
    145.85

_FHS_ ::
  VFR_Waypoint
_FHS_ =
  VFR_Waypoint
    "FARRER HIGH SCHOOL"
    (Just "NSW")
    "FHS"
    (-31.141666666666666)
    150.98333333333332

_FDP_ ::
  VFR_Waypoint
_FDP_ =
  VFR_Waypoint
    "FEDERATION PEAK"
    (Just "TAS")
    "FDP"
    (-43.266666666666666)
    146.45

_FNVL_ ::
  VFR_Waypoint
_FNVL_ =
  VFR_Waypoint
    "FERNVALE"
    (Just "QLD")
    "FNVL"
    (-27.455)
    152.65333333333334

_FLDI_ ::
  VFR_Waypoint
_FLDI_ =
  VFR_Waypoint
    "FIELD ISLAND"
    (Just "NT")
    "FLDI"
    (-12.1)
    132.38333333333333

_FISH_ ::
  VFR_Waypoint
_FISH_ =
  VFR_Waypoint
    "FISHERMANS ISLAND"
    (Just "QLD")
    "FISH"
    (-27.386666666666667)
    153.17666666666668

_FIT_ ::
  VFR_Waypoint
_FIT_ =
  VFR_Waypoint
    "FITNESS CAMP"
    (Just "NSW")
    "FIT"
    (-35.166666666666664)
    147.62166666666667

_FID_ ::
  VFR_Waypoint
_FID_ =
  VFR_Waypoint
    "FITZROY ISLAND"
    (Just "QLD")
    "FID"
    (-16.933333333333334)
    145.99166666666667

_FGN_ ::
  VFR_Waypoint
_FGN_ =
  VFR_Waypoint
    "FLEMINGTON"
    (Just "VIC")
    "FGN"
    (-37.79333333333334)
    144.91166666666666

_FPK_ ::
  VFR_Waypoint
_FPK_ =
  VFR_Waypoint
    "FLINDERS PEAK"
    (Just "QLD")
    "FPK"
    (-27.816666666666666)
    152.80833333333334

_FYN_ ::
  VFR_Waypoint
_FYN_ =
  VFR_Waypoint
    "FLYNN"
    (Just "VIC")
    "FYN"
    (-38.193333333333335)
    146.67333333333335

_FOOT_ ::
  VFR_Waypoint
_FOOT_ =
  VFR_Waypoint
    "FOOTBALL PARK"
    (Just "SA")
    "FOOT"
    (-34.88166666666667)
    138.495

_FMN_ ::
  VFR_Waypoint
_FMN_ =
  VFR_Waypoint
    "FORMARTIN"
    (Just "QLD")
    "FMN"
    (-27.39666666666667)
    151.40833333333333

_FDL_ ::
  VFR_Waypoint
_FDL_ =
  VFR_Waypoint
    "FORRESTDALE LAKE"
    (Just "WA")
    "FDL"
    (-32.16)
    115.93

_FFLD_ ::
  VFR_Waypoint
_FFLD_ =
  VFR_Waypoint
    "FORRESTFIELD"
    (Just "WA")
    "FFLD"
    (-31.988333333333333)
    116.01833333333333

_FOWB_ ::
  VFR_Waypoint
_FOWB_ =
  VFR_Waypoint
    "FOWLERS BAY"
    (Just "SA")
    "FOWB"
    (-31.983333333333334)
    132.43333333333334

_FRLG_ ::
  VFR_Waypoint
_FRLG_ =
  VFR_Waypoint
    "FREELING"
    (Just "SA")
    "FRLG"
    (-34.45666666666666)
    138.80833333333334

_FWO_ ::
  VFR_Waypoint
_FWO_ =
  VFR_Waypoint
    "FREEWAY OVERPASS"
    (Just "VIC")
    "FWO"
    (-37.795)
    144.99333333333334

_FRE_ ::
  VFR_Waypoint
_FRE_ =
  VFR_Waypoint
    "FREMANTLE"
    (Just "WA")
    "FRE"
    (-32.05833333333333)
    115.74166666666666

_FREM_ ::
  VFR_Waypoint
_FREM_ =
  VFR_Waypoint
    "FREMANTLE GOLF COURSE"
    (Just "WA")
    "FREM"
    (-32.055)
    115.77333333333333

_FRWV_ ::
  VFR_Waypoint
_FRWV_ =
  VFR_Waypoint
    "FRESHWATER VALLEY"
    (Just "QLD")
    "FRWV"
    (-16.95)
    145.68833333333333

_GALGA_ ::
  VFR_Waypoint
_GALGA_ =
  VFR_Waypoint
    "GALGA"
    (Just "SA")
    "GALGA"
    (-34.71666666666667)
    139.96666666666667

_GGN_ ::
  VFR_Waypoint
_GGN_ =
  VFR_Waypoint
    "GALLANGOWAN"
    (Just "QLD")
    "GGN"
    (-26.433333333333334)
    152.32833333333335

_GAE_ ::
  VFR_Waypoint
_GAE_ =
  VFR_Waypoint
    "GALLILEE"
    (Just "QLD")
    "GAE"
    (-22.383333333333333)
    145.98333333333332

_GWTR_ ::
  VFR_Waypoint
_GWTR_ =
  VFR_Waypoint
    "GAOL WATER TOWER"
    (Just "NT")
    "GWTR"
    (-23.858333333333334)
    133.8

_GST_ ::
  VFR_Waypoint
_GST_ =
  VFR_Waypoint
    "GATE SOUTH"
    (Just "NSW")
    "GST"
    (-31.29)
    150.685

_GWT_ ::
  VFR_Waypoint
_GWT_ =
  VFR_Waypoint
    "GATE WEST"
    (Just "NSW")
    "GWT"
    (-31.02166666666667)
    150.57166666666666

_GWB_ ::
  VFR_Waypoint
_GWB_ =
  VFR_Waypoint
    "GATEWAY BRIDGE"
    (Just "QLD")
    "GWB"
    (-27.446666666666665)
    153.1

_GEK_ ::
  VFR_Waypoint
_GEK_ =
  VFR_Waypoint
    "GEMBROOK"
    (Just "VIC")
    "GEK"
    (-37.95)
    145.55

_GRB_ ::
  VFR_Waypoint
_GRB_ =
  VFR_Waypoint
    "GEORGES RIVER BRIDGE"
    (Just "NSW")
    "GRB"
    (-34.00333333333333)
    151.11

_GEP_ ::
  VFR_Waypoint
_GEP_ =
  VFR_Waypoint
    "GEPPS CROSS"
    (Just "SA")
    "GEP"
    (-34.85)
    138.6

_GOY_ ::
  VFR_Waypoint
_GOY_ =
  VFR_Waypoint
    "GEROGERY"
    (Just "NSW")
    "GOY"
    (-35.83833333333333)
    146.99166666666667

_GNR_ ::
  VFR_Waypoint
_GNR_ =
  VFR_Waypoint
    "GIBSON REEF"
    (Just "QLD")
    "GNR"
    (-17.316666666666666)
    146.35

_GIM_ ::
  VFR_Waypoint
_GIM_ =
  VFR_Waypoint
    "GILBERLAND MINE"
    (Just "QLD")
    "GIM"
    (-19.3)
    143.6

_GIRU_ ::
  VFR_Waypoint
_GIRU_ =
  VFR_Waypoint
    "GIRU"
    (Just "QLD")
    "GIRU"
    (-19.513333333333332)
    147.105

_GVB_ ::
  VFR_Waypoint
_GVB_ =
  VFR_Waypoint
    "GLADESVILLE BRIDGE"
    (Just "NSW")
    "GVB"
    (-33.84166666666667)
    151.14666666666668

_GBRY_ ::
  VFR_Waypoint
_GBRY_ =
  VFR_Waypoint
    "GLEN BRAY"
    (Just "NSW")
    "GBRY"
    (-35.925)
    147.00833333333333

_GBR_ ::
  VFR_Waypoint
_GBR_ =
  VFR_Waypoint
    "GLENBURN"
    (Just "VIC")
    "GBR"
    (-37.425)
    145.42166666666665

_GLEN_ ::
  VFR_Waypoint
_GLEN_ =
  VFR_Waypoint
    "GLENLOCH INTERCHANGE"
    (Just "NSW")
    "GLEN"
    (-35.285)
    149.085

_GMN_ ::
  VFR_Waypoint
_GMN_ =
  VFR_Waypoint
    "GLENMORGAN"
    (Just "QLD")
    "GMN"
    (-27.25)
    149.68333333333334

_GLC_ ::
  VFR_Waypoint
_GLC_ =
  VFR_Waypoint
    "GLENORCHY"
    (Just "SA")
    "GLC"
    (-31.916666666666668)
    139.78333333333333

_GRE_ ::
  VFR_Waypoint
_GRE_ =
  VFR_Waypoint
    "GLENREAGH"
    (Just "NSW")
    "GRE"
    (-30.055)
    152.97833333333332

_GRK_ ::
  VFR_Waypoint
_GRK_ =
  VFR_Waypoint
    "GLENROCK HS"
    (Just "QLD")
    "GRK"
    (-15.116666666666667)
    145.08333333333334

_GCR_ ::
  VFR_Waypoint
_GCR_ =
  VFR_Waypoint
    "GLOUCESTER"
    (Just "NSW")
    "GCR"
    (-32.005)
    151.96666666666667

_GMH_ ::
  VFR_Waypoint
_GMH_ =
  VFR_Waypoint
    "GMH"
    (Just "VIC")
    "GMH"
    (-38.00833333333333)
    145.23833333333334

_GOI_ ::
  VFR_Waypoint
_GOI_ =
  VFR_Waypoint
    "GOLDSMITH ISLAND"
    (Just "QLD")
    "GOI"
    (-20.683333333333334)
    149.15

_GON_ ::
  VFR_Waypoint
_GON_ =
  VFR_Waypoint
    "GOODNA"
    (Just "QLD")
    "GON"
    (-27.616666666666667)
    152.88833333333332

_GGV_ ::
  VFR_Waypoint
_GGV_ =
  VFR_Waypoint
    "GOOGONG RESV"
    (Just "NSW")
    "GGV"
    (-35.42166666666667)
    149.26166666666666

_GMBG_ ::
  VFR_Waypoint
_GMBG_ =
  VFR_Waypoint
    "GOOMBUNGEE"
    (Just "QLD")
    "GMBG"
    (-27.308333333333334)
    151.85

_GNN_ ::
  VFR_Waypoint
_GNN_ =
  VFR_Waypoint
    "GOONANEMAN"
    (Just "QLD")
    "GNN"
    (-25.433333333333334)
    152.13333333333333

_GGO_ ::
  VFR_Waypoint
_GGO_ =
  VFR_Waypoint
    "GOONOO GOONOO HS"
    (Just "NSW")
    "GGO"
    (-31.31)
    150.905

_GOV_ ::
  VFR_Waypoint
_GOV_ =
  VFR_Waypoint
    "GORDONVALE"
    (Just "QLD")
    "GOV"
    (-17.088333333333335)
    145.78333333333333

_GOW_ ::
  VFR_Waypoint
_GOW_ =
  VFR_Waypoint
    "GOWRIE JUNCTION"
    (Just "QLD")
    "GOW"
    (-27.5)
    151.88666666666666

_GRAA_ ::
  VFR_Waypoint
_GRAA_ =
  VFR_Waypoint
    "GRANYA"
    (Just "VIC")
    "GRAA"
    (-36.111666666666665)
    147.31666666666666

_GVH_ ::
  VFR_Waypoint
_GVH_ =
  VFR_Waypoint
    "GRAVELLY BEACH"
    (Just "TAS")
    "GVH"
    (-41.288333333333334)
    146.97166666666666

_GRHL_ ::
  VFR_Waypoint
_GRHL_ =
  VFR_Waypoint
    "GREEN HILLS"
    (Just "WA")
    "GRHL"
    (-31.925)
    116.95833333333333

_GNIS_ ::
  VFR_Waypoint
_GNIS_ =
  VFR_Waypoint
    "GREEN ISLAND (CAIRNS)"
    (Just "QLD")
    "GNIS"
    (-16.758333333333333)
    145.975

_GRL_ ::
  VFR_Waypoint
_GRL_ =
  VFR_Waypoint
    "GREEN ISLAND (MACKAY)"
    (Just "QLD")
    "GRL"
    (-20.983333333333334)
    149.15

_GRNH_ ::
  VFR_Waypoint
_GRNH_ =
  VFR_Waypoint
    "GREENHILL"
    (Just "QLD")
    "GRNH"
    (-17.04)
    145.805

_GNM_ ::
  VFR_Waypoint
_GNM_ =
  VFR_Waypoint
    "GREENMOUNT"
    (Just "QLD")
    "GNM"
    (-27.783333333333335)
    151.95

_GRRV_ ::
  VFR_Waypoint
_GRRV_ =
  VFR_Waypoint
    "GROSE RIVER"
    (Just "NSW")
    "GRRV"
    (-33.615)
    150.67

_GDG_ ::
  VFR_Waypoint
_GDG_ =
  VFR_Waypoint
    "GUNDAGAI"
    (Just "NSW")
    "GDG"
    (-35.06166666666667)
    148.10333333333332

_GUP_ ::
  VFR_Waypoint
_GUP_ =
  VFR_Waypoint
    "GUNN PT"
    (Just "NT")
    "GUP"
    (-12.183333333333334)
    130.99166666666667

_GUNN_ ::
  VFR_Waypoint
_GUNN_ =
  VFR_Waypoint
    "GUNNING"
    (Just "NSW")
    "GUNN"
    (-34.781666666666666)
    149.26666666666668

_GUNG_ ::
  VFR_Waypoint
_GUNG_ =
  VFR_Waypoint
    "GUTHALUNGRA"
    (Just "QLD")
    "GUNG"
    (-19.933333333333334)
    147.83333333333334

_HADEN_ ::
  VFR_Waypoint
_HADEN_ =
  VFR_Waypoint
    "HADEN"
    (Just "QLD")
    "HADEN"
    (-27.22)
    151.88833333333332

_HSP_ ::
  VFR_Waypoint
_HSP_ =
  VFR_Waypoint
    "HADSPEN"
    (Just "TAS")
    "HSP"
    (-41.50833333333333)
    147.05833333333334

_HALL_ ::
  VFR_Waypoint
_HALL_ =
  VFR_Waypoint
    "HALL"
    (Just "ACT")
    "HALL"
    (-35.16833333333334)
    149.06833333333333

_HMM_ ::
  VFR_Waypoint
_HMM_ =
  VFR_Waypoint
    "HAMMOND ISLAND"
    (Just "QLD")
    "HMM"
    (-10.533333333333333)
    142.21666666666667

_HMN_ ::
  VFR_Waypoint
_HMN_ =
  VFR_Waypoint
    "HAMPTON"
    (Just "QLD")
    "HMN"
    (-27.358333333333334)
    152.06666666666666

_HAVY_ ::
  VFR_Waypoint
_HAVY_ =
  VFR_Waypoint
    "HAPPY VALLEY"
    (Just "QLD")
    "HAVY"
    (-26.808333333333334)
    153.13333333333333

_HBB_ ::
  VFR_Waypoint
_HBB_ =
  VFR_Waypoint
    "HARBOUR BRIDGE"
    (Just "NSW")
    "HBB"
    (-33.85333333333333)
    151.20833333333334

_HAF_ ::
  VFR_Waypoint
_HAF_ =
  VFR_Waypoint
    "HAREFIELD"
    (Just "NSW")
    "HAF"
    (-34.96333333333333)
    147.51666666666668

_HGTE_ ::
  VFR_Waypoint
_HGTE_ =
  VFR_Waypoint
    "HARROGATE"
    (Just "SA")
    "HGTE"
    (-34.95333333333333)
    139.01833333333335

_HARV_ ::
  VFR_Waypoint
_HARV_ =
  VFR_Waypoint
    "HARVEY"
    (Just "WA")
    "HARV"
    (-33.083333333333336)
    115.9

_HSTI_ ::
  VFR_Waypoint
_HSTI_ =
  VFR_Waypoint
    "HASTINGS"
    (Just "QLD")
    "HSTI"
    (-28.3)
    145.18333333333334

_HASS_ ::
  VFR_Waypoint
_HASS_ =
  VFR_Waypoint
    "HASTINGS PT"
    (Just "NSW")
    "HASS"
    (-28.358333333333334)
    153.58

_HATF_ ::
  VFR_Waypoint
_HATF_ =
  VFR_Waypoint
    "HATFIELD"
    (Just "NSW")
    "HATF"
    (-33.7)
    143.65

_HVI_ ::
  VFR_Waypoint
_HVI_ =
  VFR_Waypoint
    "HAVANNAH ISLAND"
    (Just "QLD")
    "HVI"
    (-18.845)
    146.53833333333333

_HPT_ ::
  VFR_Waypoint
_HPT_ =
  VFR_Waypoint
    "HAY PT"
    (Just "QLD")
    "HPT"
    (-21.278333333333332)
    149.29166666666666

_HAZ_ ::
  VFR_Waypoint
_HAZ_ =
  VFR_Waypoint
    "HAZELWOOD ISLAND"
    (Just "QLD")
    "HAZ"
    (-20.283333333333335)
    149.08333333333334

_HEAT_ ::
  VFR_Waypoint
_HEAT_ =
  VFR_Waypoint
    "HEATHCOTE TOWNSHIP"
    (Just "VIC")
    "HEAT"
    (-36.916666666666664)
    144.7

_HRR_ ::
  VFR_Waypoint
_HRR_ =
  VFR_Waypoint
    "HELENA RIVER RESV"
    (Just "WA")
    "HRR"
    (-32.001666666666665)
    116.22666666666667

_HNB_ ::
  VFR_Waypoint
_HNB_ =
  VFR_Waypoint
    "HELENSBURGH"
    (Just "NSW")
    "HNB"
    (-34.19166666666667)
    150.975

_HED_ ::
  VFR_Waypoint
_HED_ =
  VFR_Waypoint
    "HELIDON"
    (Just "QLD")
    "HED"
    (-27.55)
    152.13333333333333

_HENTY_ ::
  VFR_Waypoint
_HENTY_ =
  VFR_Waypoint
    "HENTY"
    (Just "NSW")
    "HENTY"
    (-35.525)
    147.03333333333333

_HKE_ ::
  VFR_Waypoint
_HKE_ =
  VFR_Waypoint
    "HERDSMAN LAKE"
    (Just "WA")
    "HKE"
    (-31.92)
    115.81333333333333

_HXB_ ::
  VFR_Waypoint
_HXB_ =
  VFR_Waypoint
    "HEXHAM BRIDGE"
    (Just "NSW")
    "HXB"
    (-32.83)
    151.69

_HIPA_ ::
  VFR_Waypoint
_HIPA_ =
  VFR_Waypoint
    "HIGH PERFORMANCE AREA 3"
    (Just "QLD")
    "HIPA"
    (-27.15)
    153.84666666666666

_HVTG_ ::
  VFR_Waypoint
_HVTG_ =
  VFR_Waypoint
    "HIGH VOLTAGE"
    (Just "QLD")
    "HVTG"
    (-17.015)
    145.76333333333332

_HIM_ ::
  VFR_Waypoint
_HIM_ =
  VFR_Waypoint
    "HILTON MINE"
    (Just "QLD")
    "HIM"
    (-20.566666666666666)
    139.48333333333332

_HNCH_ ::
  VFR_Waypoint
_HNCH_ =
  VFR_Waypoint
    "HINCHINBROOK ISLAND"
    (Just "QLD")
    "HNCH"
    (-18.366666666666667)
    146.25

_HDWL_ ::
  VFR_Waypoint
_HDWL_ =
  VFR_Waypoint
    "HINZE DAM WALL"
    (Just "QLD")
    "HDWL"
    (-28.05)
    153.28666666666666

_HCTY_ ::
  VFR_Waypoint
_HCTY_ =
  VFR_Waypoint
    "HOBART CBD"
    (Just "TAS")
    "HCTY"
    (-42.88333333333333)
    147.33333333333334

_HBKT_ ::
  VFR_Waypoint
_HBKT_ =
  VFR_Waypoint
    "HOLBROOK TOWNSHIP"
    (Just "NSW")
    "HBKT"
    (-35.73)
    147.325

_HOLM_ ::
  VFR_Waypoint
_HOLM_ =
  VFR_Waypoint
    "HOLMES REEF"
    (Just "QLD")
    "HOLM"
    (-16.5)
    148.0

_HBU_ ::
  VFR_Waypoint
_HBU_ =
  VFR_Waypoint
    "HOMEBUSH"
    (Just "QLD")
    "HBU"
    (-21.28)
    149.05

_HDP_ ::
  VFR_Waypoint
_HDP_ =
  VFR_Waypoint
    "HOOD PT"
    (Just "WA")
    "HDP"
    (-34.38333333333333)
    119.56666666666666

_HPI_ ::
  VFR_Waypoint
_HPI_ =
  VFR_Waypoint
    "HOPE INLET"
    (Just "NT")
    "HPI"
    (-12.328333333333333)
    131.01666666666668

_HVR_ ::
  VFR_Waypoint
_HVR_ =
  VFR_Waypoint
    "HOPE VALLEY RESV"
    (Just "SA")
    "HVR"
    (-34.85)
    138.68333333333334

_HORD_ ::
  VFR_Waypoint
_HORD_ =
  VFR_Waypoint
    "HORDERN HILLS"
    (Just "NT")
    "HORD"
    (-20.65)
    130.31666666666666

_HZWF_ ::
  VFR_Waypoint
_HZWF_ =
  VFR_Waypoint
    "HORIZONTAL WATERFALLS"
    (Just "WA")
    "HZWF"
    (-16.383333333333333)
    123.96666666666667

_HBVT_ ::
  VFR_Waypoint
_HBVT_ =
  VFR_Waypoint
    "HORNIBROOK VIADUCT"
    (Just "QLD")
    "HBVT"
    (-27.273333333333333)
    153.07166666666666

_HSY_ ::
  VFR_Waypoint
_HSY_ =
  VFR_Waypoint
    "HORNSBY"
    (Just "NSW")
    "HSY"
    (-33.69166666666667)
    151.10666666666665

_HST_ ::
  VFR_Waypoint
_HST_ =
  VFR_Waypoint
    "HOSKINSTOWN"
    (Just "NSW")
    "HST"
    (-35.42166666666667)
    149.45

_HWG_ ::
  VFR_Waypoint
_HWG_ =
  VFR_Waypoint
    "HOWLONG"
    (Just "NSW")
    "HWG"
    (-35.97666666666667)
    146.625

_HGR_ ::
  VFR_Waypoint
_HGR_ =
  VFR_Waypoint
    "HUGH RIVER"
    (Just "NT")
    "HGR"
    (-24.35)
    133.43333333333334

_HWW_ ::
  VFR_Waypoint
_HWW_ =
  VFR_Waypoint
    "HUME WEIR WALL"
    (Just "VIC")
    "HWW"
    (-36.111666666666665)
    147.025

_HYH_ ::
  VFR_Waypoint
_HYH_ =
  VFR_Waypoint
    "HUMMOCKY HILLS"
    (Just "TAS")
    "HYH"
    (-41.733333333333334)
    147.25

_HRD_ ::
  VFR_Waypoint
_HRD_ =
  VFR_Waypoint
    "HUNGERFORD"
    (Just "NSW")
    "HRD"
    (-29.0)
    144.4

_HYDEN_ ::
  VFR_Waypoint
_HYDEN_ =
  VFR_Waypoint
    "HYDEN"
    (Just "WA")
    "HYDEN"
    (-32.45)
    118.86666666666666

_IND_ ::
  VFR_Waypoint
_IND_ =
  VFR_Waypoint
    "INDEE HS"
    (Just "WA")
    "IND"
    (-20.786666666666665)
    118.59166666666667

_IDNA_ ::
  VFR_Waypoint
_IDNA_ =
  VFR_Waypoint
    "INDIANA"
    (Just "NT")
    "IDNA"
    (-23.333333333333332)
    135.43333333333334

_IDK_ ::
  VFR_Waypoint
_IDK_ =
  VFR_Waypoint
    "INDULKANA TOWNSHIP"
    (Just "SA")
    "IDK"
    (-26.966666666666665)
    133.325

_INGL_ ::
  VFR_Waypoint
_INGL_ =
  VFR_Waypoint
    "INGLEBURN"
    (Just "NSW")
    "INGL"
    (-33.971666666666664)
    150.85833333333332

_IPHL_ ::
  VFR_Waypoint
_IPHL_ =
  VFR_Waypoint
    "IPPIA HILL"
    (Just "NT")
    "IPHL"
    (-25.116666666666667)
    133.05

_ISB_ ::
  VFR_Waypoint
_ISB_ =
  VFR_Waypoint
    "ISRAELITE BAY"
    (Just "WA")
    "ISB"
    (-33.61666666666667)
    123.88333333333334

_JSK_ ::
  VFR_Waypoint
_JSK_ =
  VFR_Waypoint
    "JACK SMITH LAKE"
    (Just "VIC")
    "JSK"
    (-38.5)
    147.0

_JAC_ ::
  VFR_Waypoint
_JAC_ =
  VFR_Waypoint
    "JACKO'S JUNCTION"
    (Just "NT")
    "JAC"
    (-12.263333333333334)
    131.03666666666666

_JNR_ ::
  VFR_Waypoint
_JNR_ =
  VFR_Waypoint
    "JACKSON RIVER"
    (Just "QLD")
    "JNR"
    (-11.583333333333334)
    142.0

_JSL_ ::
  VFR_Waypoint
_JSL_ =
  VFR_Waypoint
    "JACOB'S SUGARLOAF"
    (Just "TAS")
    "JSL"
    (-41.94166666666667)
    147.3

_JACW_ ::
  VFR_Waypoint
_JACW_ =
  VFR_Waypoint
    "JACOB'S WELL"
    (Just "WA")
    "JACW"
    (-32.03333333333333)
    117.2

_JMPP_ ::
  VFR_Waypoint
_JMPP_ =
  VFR_Waypoint
    "JAMES PRICE POINT"
    (Just "WA")
    "JMPP"
    (-17.485)
    122.14333333333333

_JADL_ ::
  VFR_Waypoint
_JADL_ =
  VFR_Waypoint
    "JARRAHDALE"
    (Just "WA")
    "JADL"
    (-32.34)
    116.075

_JCK_ ::
  VFR_Waypoint
_JCK_ =
  VFR_Waypoint
    "JAY CREEK"
    (Just "NT")
    "JCK"
    (-23.786666666666665)
    133.5

_JEA_ ::
  VFR_Waypoint
_JEA_ =
  VFR_Waypoint
    "JEANNIE RIVER"
    (Just "QLD")
    "JEA"
    (-14.733333333333333)
    144.86666666666667

_JES_ ::
  VFR_Waypoint
_JES_ =
  VFR_Waypoint
    "JESSIE GAP"
    (Just "NT")
    "JES"
    (-23.748333333333335)
    134.01833333333335

_JIBN_ ::
  VFR_Waypoint
_JIBN_ =
  VFR_Waypoint
    "JIBBON PT"
    (Just "NSW")
    "JIBN"
    (-34.085)
    151.17

_JIA_ ::
  VFR_Waypoint
_JIA_ =
  VFR_Waypoint
    "JINDERA"
    (Just "NSW")
    "JIA"
    (-35.958333333333336)
    146.88833333333332

_JDN_ ::
  VFR_Waypoint
_JDN_ =
  VFR_Waypoint
    "JONDARYAN"
    (Just "QLD")
    "JDN"
    (-27.366666666666667)
    151.58833333333334

_JGK_ ::
  VFR_Waypoint
_JGK_ =
  VFR_Waypoint
    "JUG CREEK"
    (Just "QLD")
    "JGK"
    (-22.0)
    144.7

_JPP_ ::
  VFR_Waypoint
_JPP_ =
  VFR_Waypoint
    "JUMPINPIN"
    (Just "QLD")
    "JPP"
    (-27.733333333333334)
    153.45

_JUNEE_ ::
  VFR_Waypoint
_JUNEE_ =
  VFR_Waypoint
    "JUNEE"
    (Just "NSW")
    "JUNEE"
    (-34.86666666666667)
    147.58333333333334

_JUP_ ::
  VFR_Waypoint
_JUP_ =
  VFR_Waypoint
    "JUPITERS CASINO"
    (Just "QLD")
    "JUP"
    (-28.031666666666666)
    153.43

_KKN_ ::
  VFR_Waypoint
_KKN_ =
  VFR_Waypoint
    "KAIMKILLENBUN"
    (Just "QLD")
    "KKN"
    (-27.066666666666666)
    151.43333333333334

_KAO_ ::
  VFR_Waypoint
_KAO_ =
  VFR_Waypoint
    "KALKALLO"
    (Just "VIC")
    "KAO"
    (-37.53666666666667)
    144.945

_KTS_ ::
  VFR_Waypoint
_KTS_ =
  VFR_Waypoint
    "KEATS ISLAND"
    (Just "QLD")
    "KTS"
    (-9.683333333333334)
    143.45

_KEP_ ::
  VFR_Waypoint
_KEP_ =
  VFR_Waypoint
    "KEEPIT DAM"
    (Just "NSW")
    "KEP"
    (-30.88)
    150.49833333333333

_KERW_ ::
  VFR_Waypoint
_KERW_ =
  VFR_Waypoint
    "KEERWEE"
    (Just "QLD")
    "KERW"
    (-25.216666666666665)
    151.35

_KALL_ ::
  VFR_Waypoint
_KALL_ =
  VFR_Waypoint
    "KENDALL"
    (Just "QLD")
    "KALL"
    (-14.2)
    141.6

_KSI_ ::
  VFR_Waypoint
_KSI_ =
  VFR_Waypoint
    "KESWICK ISLAND"
    (Just "QLD")
    "KSI"
    (-20.916666666666668)
    149.41666666666666

_KMA_ ::
  VFR_Waypoint
_KMA_ =
  VFR_Waypoint
    "KIAMA"
    (Just "NSW")
    "KMA"
    (-34.666666666666664)
    150.85

_KIAN_ ::
  VFR_Waypoint
_KIAN_ =
  VFR_Waypoint
    "KIANDRA"
    (Just "NSW")
    "KIAN"
    (-35.86666666666667)
    148.5

_KDBF_ ::
  VFR_Waypoint
_KDBF_ =
  VFR_Waypoint
    "KIDSON BLUFF"
    (Just "WA")
    "KDBF"
    (-22.25)
    125.03333333333333

_KIEWA_ ::
  VFR_Waypoint
_KIEWA_ =
  VFR_Waypoint
    "KIEWA"
    (Just "VIC")
    "KIEWA"
    (-36.25833333333333)
    147.00833333333333

_KLCY_ ::
  VFR_Waypoint
_KLCY_ =
  VFR_Waypoint
    "KILCOY TOWNSHIP"
    (Just "QLD")
    "KLCY"
    (-26.941666666666666)
    152.56333333333333

_KKV_ ::
  VFR_Waypoint
_KKV_ =
  VFR_Waypoint
    "KILKIVAN"
    (Just "QLD")
    "KKV"
    (-26.083333333333332)
    152.24666666666667

_KIM_ ::
  VFR_Waypoint
_KIM_ =
  VFR_Waypoint
    "KILMORE"
    (Just "VIC")
    "KIM"
    (-37.3)
    144.955

_KMG_ ::
  VFR_Waypoint
_KMG_ =
  VFR_Waypoint
    "KILMORE GAP"
    (Just "VIC")
    "KMG"
    (-37.3)
    144.98333333333332

_KLTO_ ::
  VFR_Waypoint
_KLTO_ =
  VFR_Waypoint
    "KILTO"
    (Just "WA")
    "KLTO"
    (-17.691666666666666)
    122.71333333333334

_KCAS_ ::
  VFR_Waypoint
_KCAS_ =
  VFR_Waypoint
    "KING CASCADES"
    (Just "WA")
    "KCAS"
    (-15.625)
    125.3

_KRT_ ::
  VFR_Waypoint
_KRT_ =
  VFR_Waypoint
    "KING RANCH TULLY"
    (Just "QLD")
    "KRT"
    (-18.083333333333332)
    145.83333333333334

_KGLE_ ::
  VFR_Waypoint
_KGLE_ =
  VFR_Waypoint
    "KINGLAKE"
    (Just "VIC")
    "KGLE"
    (-37.52166666666667)
    145.35

_KBCH_ ::
  VFR_Waypoint
_KBCH_ =
  VFR_Waypoint
    "KINGS BEACH"
    (Just "QLD")
    "KBCH"
    (-26.805)
    153.14

_KCFF_ ::
  VFR_Waypoint
_KCFF_ =
  VFR_Waypoint
    "KINGSCLIFF"
    (Just "NSW")
    "KCFF"
    (-28.25)
    153.57166666666666

_KGT_ ::
  VFR_Waypoint
_KGT_ =
  VFR_Waypoint
    "KINGSTHORPE"
    (Just "QLD")
    "KGT"
    (-27.47833333333333)
    151.81333333333333

_KINN_ ::
  VFR_Waypoint
_KINN_ =
  VFR_Waypoint
    "KINGSTON CENTRE"
    (Just "VIC")
    "KINN"
    (-37.95666666666666)
    145.07666666666665

_KIRA_ ::
  VFR_Waypoint
_KIRA_ =
  VFR_Waypoint
    "KIRRA"
    (Just "NSW")
    "KIRA"
    (-28.166666666666668)
    153.52333333333334

_KSPT_ ::
  VFR_Waypoint
_KSPT_ =
  VFR_Waypoint
    "KISSING POINT"
    (Just "QLD")
    "KSPT"
    (-19.241666666666667)
    146.805

_KNW_ ::
  VFR_Waypoint
_KNW_ =
  VFR_Waypoint
    "KONGWAK"
    (Just "VIC")
    "KNW"
    (-38.516666666666666)
    145.71666666666667

_KTG_ ::
  VFR_Waypoint
_KTG_ =
  VFR_Waypoint
    "KOOLATONG RIVER"
    (Just "NT")
    "KTG"
    (-13.083333333333334)
    135.65

_KBD_ ::
  VFR_Waypoint
_KBD_ =
  VFR_Waypoint
    "KOOMBOOLOOMBA DAM"
    (Just "QLD")
    "KBD"
    (-17.833333333333332)
    145.6

_KOOM_ ::
  VFR_Waypoint
_KOOM_ =
  VFR_Waypoint
    "KOOMOOLOOBOOKA CAVE"
    (Just "SA")
    "KOOM"
    (-31.483333333333334)
    129.58333333333334

_KANC_ ::
  VFR_Waypoint
_KANC_ =
  VFR_Waypoint
    "KOORAN CROCODILE FARM"
    (Just "QLD")
    "KANC"
    (-23.3)
    150.73333333333332

_KOT_ ::
  VFR_Waypoint
_KOT_ =
  VFR_Waypoint
    "KOOTINGAL"
    (Just "NSW")
    "KOT"
    (-31.05)
    151.05

_KOPP_ ::
  VFR_Waypoint
_KOPP_ =
  VFR_Waypoint
    "KOPPEN PARK"
    (Just "QLD")
    "KOPP"
    (-16.931666666666665)
    145.73333333333332

_KREE_ ::
  VFR_Waypoint
_KREE_ =
  VFR_Waypoint
    "KOREELAH"
    (Just "SA")
    "KREE"
    (-35.916666666666664)
    136.91666666666666

_KUN_ ::
  VFR_Waypoint
_KUN_ =
  VFR_Waypoint
    "KUNWARARA"
    (Just "QLD")
    "KUN"
    (-22.916666666666668)
    150.13333333333333

_KRN_ ::
  VFR_Waypoint
_KRN_ =
  VFR_Waypoint
    "KURANDA"
    (Just "QLD")
    "KRN"
    (-16.816666666666666)
    145.63833333333332

_KRMD_ ::
  VFR_Waypoint
_KRMD_ =
  VFR_Waypoint
    "KURMOND"
    (Just "NSW")
    "KRMD"
    (-33.55)
    150.68666666666667

_KYP_ ::
  VFR_Waypoint
_KYP_ =
  VFR_Waypoint
    "KYEAMBA PARK"
    (Just "NSW")
    "KYP"
    (-35.446666666666665)
    147.61666666666667

_KYE_ ::
  VFR_Waypoint
_KYE_ =
  VFR_Waypoint
    "KYEAMBA TOWER"
    (Just "NSW")
    "KYE"
    (-35.525)
    147.59166666666667

_KTN_ ::
  VFR_Waypoint
_KTN_ =
  VFR_Waypoint
    "KYNETON"
    (Just "VIC")
    "KTN"
    (-37.24666666666667)
    144.45833333333334

_LDH_ ::
  VFR_Waypoint
_LDH_ =
  VFR_Waypoint
    "LADYSMITH"
    (Just "NSW")
    "LDH"
    (-35.211666666666666)
    147.51666666666668

_LDLY_ ::
  VFR_Waypoint
_LDLY_ =
  VFR_Waypoint
    "LAIDLEY"
    (Just "QLD")
    "LDLY"
    (-27.633333333333333)
    152.38333333333333

_LKT_ ::
  VFR_Waypoint
_LKT_ =
  VFR_Waypoint
    "LAKE ALBERT"
    (Just "NSW")
    "LKT"
    (-35.178333333333335)
    147.36333333333334

_LKAD_ ::
  VFR_Waypoint
_LKAD_ =
  VFR_Waypoint
    "LAKE AMADEUS"
    (Just "NT")
    "LKAD"
    (-24.58)
    130.46166666666667

_LBT_ ::
  VFR_Waypoint
_LBT_ =
  VFR_Waypoint
    "LAKE BATHURST"
    (Just "NSW")
    "LBT"
    (-35.05)
    149.68333333333334

_LKON_ ::
  VFR_Waypoint
_LKON_ =
  VFR_Waypoint
    "LAKE CLARENDON"
    (Just "QLD")
    "LKON"
    (-27.491666666666667)
    152.35

_LCR_ ::
  VFR_Waypoint
_LCR_ =
  VFR_Waypoint
    "LAKE CORANGAMITE"
    (Just "VIC")
    "LCR"
    (-38.1)
    143.51666666666668

_LCDI_ ::
  VFR_Waypoint
_LCDI_ =
  VFR_Waypoint
    "LAKE CURRIMUNDI"
    (Just "QLD")
    "LCDI"
    (-26.765)
    153.13666666666666

_LAD_ ::
  VFR_Waypoint
_LAD_ =
  VFR_Waypoint
    "LAKE DEAN"
    (Just "NT")
    "LAD"
    (-12.733333333333333)
    131.01666666666668

_LDAP_ ::
  VFR_Waypoint
_LDAP_ =
  VFR_Waypoint
    "LAKE DISAPPOINTMENT"
    (Just "WA")
    "LDAP"
    (-23.5)
    122.66666666666667

_LKEC_ ::
  VFR_Waypoint
_LKEC_ =
  VFR_Waypoint
    "LAKE ECHO"
    (Just "TAS")
    "LKEC"
    (-42.166666666666664)
    146.63333333333333

_LEM_ ::
  VFR_Waypoint
_LEM_ =
  VFR_Waypoint
    "LAKE EPSOM"
    (Just "QLD")
    "LEM"
    (-21.488333333333333)
    148.83333333333334

_LEYN_ ::
  VFR_Waypoint
_LEYN_ =
  VFR_Waypoint
    "LAKE EYRE NORTH"
    (Just "SA")
    "LEYN"
    (-28.416666666666668)
    137.3

_LFS_ ::
  VFR_Waypoint
_LFS_ =
  VFR_Waypoint
    "LAKE FINNISS"
    (Just "NT")
    "LFS"
    (-12.38)
    131.475

_LFRO_ ::
  VFR_Waypoint
_LFRO_ =
  VFR_Waypoint
    "LAKE FROME"
    (Just "SA")
    "LFRO"
    (-30.633333333333333)
    139.86666666666667

_LGGN_ ::
  VFR_Waypoint
_LGGN_ =
  VFR_Waypoint
    "LAKE GEORGE NORTH"
    (Just "NSW")
    "LGGN"
    (-34.98833333333333)
    149.39166666666668

_LGGS_ ::
  VFR_Waypoint
_LGGS_ =
  VFR_Waypoint
    "LAKE GEORGE SOUTH"
    (Just "NSW")
    "LGGS"
    (-35.20333333333333)
    149.40833333333333

_LGEN_ ::
  VFR_Waypoint
_LGEN_ =
  VFR_Waypoint
    "LAKE GILLEN"
    (Just "WA")
    "LGEN"
    (-26.216666666666665)
    124.6

_LGDA_ ::
  VFR_Waypoint
_LGDA_ =
  VFR_Waypoint
    "LAKE GININDERRA"
    (Just "ACT")
    "LGDA"
    (-35.233333333333334)
    149.06833333333333

_LKH_ ::
  VFR_Waypoint
_LKH_ =
  VFR_Waypoint
    "LAKE HINDMARSH"
    (Just "VIC")
    "LKH"
    (-36.11666666666667)
    141.86666666666667

_LKIM_ ::
  VFR_Waypoint
_LKIM_ =
  VFR_Waypoint
    "LAKE ILMA"
    (Just "WA")
    "LKIM"
    (-29.25)
    127.76666666666667

_LKG_ ::
  VFR_Waypoint
_LKG_ =
  VFR_Waypoint
    "LAKE KING"
    (Just "WA")
    "LKG"
    (-33.083333333333336)
    119.66666666666667

_LMC_ ::
  VFR_Waypoint
_LMC_ =
  VFR_Waypoint
    "LAKE MANCHESTER"
    (Just "QLD")
    "LMC"
    (-27.483333333333334)
    152.76666666666668

_LME_ ::
  VFR_Waypoint
_LME_ =
  VFR_Waypoint
    "LAKE MAURICE"
    (Just "SA")
    "LME"
    (-29.4)
    130.95

_LMWL_ ::
  VFR_Waypoint
_LMWL_ =
  VFR_Waypoint
    "LAKE MINIGWAL"
    (Just "WA")
    "LMWL"
    (-29.583333333333332)
    123.16666666666667

_LOOH_ ::
  VFR_Waypoint
_LOOH_ =
  VFR_Waypoint
    "LAKE MOOGERAH"
    (Just "QLD")
    "LOOH"
    (-28.033333333333335)
    152.55

_LKNL_ ::
  VFR_Waypoint
_LKNL_ =
  VFR_Waypoint
    "LAKE NEALE"
    (Just "NT")
    "LKNL"
    (-24.246666666666666)
    129.96666666666667

_LRID_ ::
  VFR_Waypoint
_LRID_ =
  VFR_Waypoint
    "LAKE RAESIDE"
    (Just "WA")
    "LRID"
    (-29.55)
    122.3

_LRAN_ ::
  VFR_Waypoint
_LRAN_ =
  VFR_Waypoint
    "LAKE RASON"
    (Just "WA")
    "LRAN"
    (-28.666666666666668)
    124.28333333333333

_LSPR_ ::
  VFR_Waypoint
_LSPR_ =
  VFR_Waypoint
    "LAKE SURPRISE"
    (Just "NT")
    "LSPR"
    (-20.233333333333334)
    131.8

_LTOM_ ::
  VFR_Waypoint
_LTOM_ =
  VFR_Waypoint
    "LAKE THOMSON"
    (Just "WA")
    "LTOM"
    (-32.151666666666664)
    115.835

_LTL_ ::
  VFR_Waypoint
_LTL_ =
  VFR_Waypoint
    "LAKE THROSSELL"
    (Just "WA")
    "LTL"
    (-27.633333333333333)
    124.08333333333333

_LTRR_ ::
  VFR_Waypoint
_LTRR_ =
  VFR_Waypoint
    "LAKE TORRENS"
    (Just "SA")
    "LTRR"
    (-31.421666666666667)
    138.08666666666667

_WITE_ ::
  VFR_Waypoint
_WITE_ =
  VFR_Waypoint
    "LAKE WHITE"
    (Just "NT")
    "WITE"
    (-21.1)
    129.05

_LYEO_ ::
  VFR_Waypoint
_LYEO_ =
  VFR_Waypoint
    "LAKE YEO"
    (Just "WA")
    "LYEO"
    (-28.051666666666666)
    124.55166666666666

_LKEE_ ::
  VFR_Waypoint
_LKEE_ =
  VFR_Waypoint
    "LAKES ENTRANCE"
    (Just "VIC")
    "LKEE"
    (-37.86666666666667)
    148.0

_LCD_ ::
  VFR_Waypoint
_LCD_ =
  VFR_Waypoint
    "LANCEFIELD"
    (Just "VIC")
    "LCD"
    (-37.278333333333336)
    144.72833333333332

_LANC_ ::
  VFR_Waypoint
_LANC_ =
  VFR_Waypoint
    "LANCELIN TOWNSHIP"
    (Just "WA")
    "LANC"
    (-31.018333333333334)
    115.32833333333333

_LGH_ ::
  VFR_Waypoint
_LGH_ =
  VFR_Waypoint
    "LANGHAM"
    (Just "QLD")
    "LGH"
    (-22.2)
    150.1

_LHCK_ ::
  VFR_Waypoint
_LHCK_ =
  VFR_Waypoint
    "LANGHORNE CREEK"
    (Just "SA")
    "LHCK"
    (-35.29666666666667)
    139.045

_LSDW_ ::
  VFR_Waypoint
_LSDW_ =
  VFR_Waypoint
    "LANSDOWNE"
    (Just "WA")
    "LSDW"
    (-17.615)
    126.74333333333334

_LUY_ ::
  VFR_Waypoint
_LUY_ =
  VFR_Waypoint
    "LATROBE UNIVERSITY"
    (Just "VIC")
    "LUY"
    (-37.71666666666667)
    145.05

_LAUD_ ::
  VFR_Waypoint
_LAUD_ =
  VFR_Waypoint
    "LAUDERDALE"
    (Just "TAS")
    "LAUD"
    (-42.9)
    147.5

_LVAB_ ::
  VFR_Waypoint
_LVAB_ =
  VFR_Waypoint
    "LAVERTON"
    (Just "VIC")
    "LVAB"
    (-37.86333333333333)
    144.745

_TON_ ::
  VFR_Waypoint
_TON_ =
  VFR_Waypoint
    "LAVERTON BOM TOWER"
    (Just "VIC")
    "TON"
    (-37.855)
    144.755

_LWG_ ::
  VFR_Waypoint
_LWG_ =
  VFR_Waypoint
    "LAWRENCE GORGE"
    (Just "NT")
    "LWG"
    (-24.016666666666666)
    133.40833333333333

_LAOS_ ::
  VFR_Waypoint
_LAOS_ =
  VFR_Waypoint
    "LAYOAK ISLAND"
    (Just "QLD")
    "LAOS"
    (-9.85)
    143.31666666666666

_LPT_ ::
  VFR_Waypoint
_LPT_ =
  VFR_Waypoint
    "LEE PT"
    (Just "NT")
    "LPT"
    (-12.333333333333334)
    130.9

_LPD_ ::
  VFR_Waypoint
_LPD_ =
  VFR_Waypoint
    "LEOPOLD"
    (Just "VIC")
    "LPD"
    (-38.19166666666667)
    144.46666666666667

_LIHR_ ::
  VFR_Waypoint
_LIHR_ =
  VFR_Waypoint
    "LIGHTHORSE INTERCHANGE M7/M4"
    (Just "NSW")
    "LIHR"
    (-33.79833333333333)
    150.85333333333332

_LIY_ ::
  VFR_Waypoint
_LIY_ =
  VFR_Waypoint
    "LILYDALE"
    (Just "TAS")
    "LIY"
    (-41.25)
    147.21666666666667

_LOWS_ ::
  VFR_Waypoint
_LOWS_ =
  VFR_Waypoint
    "LINDENOW SOUTH"
    (Just "VIC")
    "LOWS"
    (-37.82833333333333)
    147.43333333333334

_LVE_ ::
  VFR_Waypoint
_LVE_ =
  VFR_Waypoint
    "LINVILLE"
    (Just "QLD")
    "LVE"
    (-26.85)
    152.26666666666668

_LMGE_ ::
  VFR_Waypoint
_LMGE_ =
  VFR_Waypoint
    "LITTLE MULGRAVE"
    (Just "QLD")
    "LMGE"
    (-17.138333333333332)
    145.725

_LRM_ ::
  VFR_Waypoint
_LRM_ =
  VFR_Waypoint
    "LITTLE RIVER MOUTH"
    (Just "VIC")
    "LRM"
    (-38.00666666666667)
    144.58333333333334

_LBET_ ::
  VFR_Waypoint
_LBET_ =
  VFR_Waypoint
    "LOBETHAL"
    (Just "SA")
    "LBET"
    (-34.9)
    138.86666666666667

_LSR_ ::
  VFR_Waypoint
_LSR_ =
  VFR_Waypoint
    "LOCH SPORT"
    (Just "VIC")
    "LSR"
    (-38.05)
    147.58333333333334

_LOGI_ ::
  VFR_Waypoint
_LOGI_ =
  VFR_Waypoint
    "LOGIC CENTRE"
    (Just "VIC")
    "LOGI"
    (-36.07666666666667)
    146.71833333333333

_LRF_ ::
  VFR_Waypoint
_LRF_ =
  VFR_Waypoint
    "LONG REEF"
    (Just "NSW")
    "LRF"
    (-33.74166666666667)
    151.32166666666666

_LFC_ ::
  VFR_Waypoint
_LFC_ =
  VFR_Waypoint
    "LONGFORD CREEK"
    (Just "QLD")
    "LFC"
    (-20.208333333333332)
    148.36666666666667

_LORN_ ::
  VFR_Waypoint
_LORN_ =
  VFR_Waypoint
    "LORNE TOWNSHIP"
    (Just "VIC")
    "LORN"
    (-38.54333333333334)
    143.97

_LHD_ ::
  VFR_Waypoint
_LHD_ =
  VFR_Waypoint
    "LOW HEAD"
    (Just "TAS")
    "LHD"
    (-41.06666666666667)
    146.8

_LWI_ ::
  VFR_Waypoint
_LWI_ =
  VFR_Waypoint
    "LOW ISLETS"
    (Just "QLD")
    "LWI"
    (-16.383333333333333)
    145.56666666666666

_LRP_ ::
  VFR_Waypoint
_LRP_ =
  VFR_Waypoint
    "LOW ROCKY PT"
    (Just "TAS")
    "LRP"
    (-43.0)
    145.5

_LWD_ ::
  VFR_Waypoint
_LWD_ =
  VFR_Waypoint
    "LOWOOD"
    (Just "QLD")
    "LWD"
    (-27.466666666666665)
    152.58333333333334

_LNDA_ ::
  VFR_Waypoint
_LNDA_ =
  VFR_Waypoint
    "LUCINDA"
    (Just "QLD")
    "LNDA"
    (-18.533333333333335)
    146.33333333333334

_LYNR_ ::
  VFR_Waypoint
_LYNR_ =
  VFR_Waypoint
    "LYND RIVER"
    (Just "QLD")
    "LYNR"
    (-17.4)
    143.75

_MACB_ ::
  VFR_Waypoint
_MACB_ =
  VFR_Waypoint
    "MACHANS BEACH"
    (Just "QLD")
    "MACB"
    (-16.85)
    145.75

_MZR_ ::
  VFR_Waypoint
_MZR_ =
  VFR_Waypoint
    "MACKENZIE RIVER"
    (Just "QLD")
    "MZR"
    (-23.166666666666668)
    149.5

_MKV_ ::
  VFR_Waypoint
_MKV_ =
  VFR_Waypoint
    "MACKSVILLE"
    (Just "NSW")
    "MKV"
    (-30.708333333333332)
    152.915

_MGEW_ ::
  VFR_Waypoint
_MGEW_ =
  VFR_Waypoint
    "MAGILL ESTATE WINERY"
    (Just "SA")
    "MGEW"
    (-34.92)
    138.68

_MAS_ ::
  VFR_Waypoint
_MAS_ =
  VFR_Waypoint
    "MAGNESITE MINE"
    (Just "QLD")
    "MAS"
    (-22.888333333333332)
    150.18333333333334

_MADO_ ::
  VFR_Waypoint
_MADO_ =
  VFR_Waypoint
    "MANDORAH"
    (Just "NT")
    "MADO"
    (-12.441666666666666)
    130.76166666666666

_MDU_ ::
  VFR_Waypoint
_MDU_ =
  VFR_Waypoint
    "MANDURAH"
    (Just "WA")
    "MDU"
    (-32.53)
    115.72166666666666

_MGL_ ::
  VFR_Waypoint
_MGL_ =
  VFR_Waypoint
    "MANGALORE"
    (Just "TAS")
    "MGL"
    (-42.65833333333333)
    147.24166666666667

_MOP_ ::
  VFR_Waypoint
_MOP_ =
  VFR_Waypoint
    "MANGOPLAH"
    (Just "NSW")
    "MOP"
    (-35.391666666666666)
    147.24166666666667

_MAL_ ::
  VFR_Waypoint
_MAL_ =
  VFR_Waypoint
    "MANILLA"
    (Just "NSW")
    "MAL"
    (-30.75)
    150.72333333333333

_MANLY_ ::
  VFR_Waypoint
_MANLY_ =
  VFR_Waypoint
    "MANLY"
    (Just "NSW")
    "MANLY"
    (-33.79833333333333)
    151.28833333333333

_MTD_ ::
  VFR_Waypoint
_MTD_ =
  VFR_Waypoint
    "MANTON DAM"
    (Just "NT")
    "MTD"
    (-12.85)
    131.125

_MBA_ ::
  VFR_Waypoint
_MBA_ =
  VFR_Waypoint
    "MAREEBA"
    (Just "QLD")
    "MBA"
    (-17.06833333333333)
    145.41833333333332

_MRL_ ::
  VFR_Waypoint
_MRL_ =
  VFR_Waypoint
    "MARIA ISLAND"
    (Just "TAS")
    "MRL"
    (-42.63333333333333)
    148.08333333333334

_MLIT_ ::
  VFR_Waypoint
_MLIT_ =
  VFR_Waypoint
    "MARINO LIGHT HOUSE"
    (Just "SA")
    "MLIT"
    (-35.055)
    138.51333333333332

_MARQ_ ::
  VFR_Waypoint
_MARQ_ =
  VFR_Waypoint
    "MARINO'S QUARRY"
    (Just "QLD")
    "MARQ"
    (-16.92)
    145.72666666666666

_MRBR_ ::
  VFR_Waypoint
_MRBR_ =
  VFR_Waypoint
    "MAROUBRA BEACH"
    (Just "NSW")
    "MRBR"
    (-33.95)
    151.25666666666666

_MAR_ ::
  VFR_Waypoint
_MAR_ =
  VFR_Waypoint
    "MARRAR"
    (Just "NSW")
    "MAR"
    (-34.825)
    147.35

_MARR_ ::
  VFR_Waypoint
_MARR_ =
  VFR_Waypoint
    "MARRAWAH"
    (Just "TAS")
    "MARR"
    (-40.91)
    144.70833333333334

_MHT_ ::
  VFR_Waypoint
_MHT_ =
  VFR_Waypoint
    "MARTHA PT"
    (Just "VIC")
    "MHT"
    (-38.31666666666667)
    144.98333333333332

_MKN_ ::
  VFR_Waypoint
_MKN_ =
  VFR_Waypoint
    "MARY KATHLEEN"
    (Just "QLD")
    "MKN"
    (-20.783333333333335)
    139.98333333333332

_MVL_ ::
  VFR_Waypoint
_MVL_ =
  VFR_Waypoint
    "MARYSVILLE"
    (Just "VIC")
    "MVL"
    (-37.516666666666666)
    145.75

_MHD_ ::
  VFR_Waypoint
_MHD_ =
  VFR_Waypoint
    "MASTHEAD ISLAND"
    (Just "QLD")
    "MHD"
    (-23.533333333333335)
    151.73333333333332

_MRH_ ::
  VFR_Waypoint
_MRH_ =
  VFR_Waypoint
    "MAURICE HILL"
    (Just "QLD")
    "MRH"
    (-23.933333333333334)
    151.25

_MAYD_ ::
  VFR_Waypoint
_MAYD_ =
  VFR_Waypoint
    "MAYDENA"
    (Just "TAS")
    "MAYD"
    (-42.763333333333335)
    146.59666666666666

_MYF_ ::
  VFR_Waypoint
_MYF_ =
  VFR_Waypoint
    "MAYFIELD"
    (Just "NSW")
    "MYF"
    (-33.96333333333333)
    150.625

_MAYL_ ::
  VFR_Waypoint
_MAYL_ =
  VFR_Waypoint
    "MAYLANDS POLICE ACADEMY"
    (Just "WA")
    "MAYL"
    (-31.946666666666665)
    115.90333333333334

_MVAL_ ::
  VFR_Waypoint
_MVAL_ =
  VFR_Waypoint
    "MCLAREN VALE"
    (Just "SA")
    "MVAL"
    (-35.225)
    138.54666666666665

_MCTY_ ::
  VFR_Waypoint
_MCTY_ =
  VFR_Waypoint
    "MELBOURNE CBD"
    (Just "VIC")
    "MCTY"
    (-37.80833333333333)
    144.95833333333334

_MCG_ ::
  VFR_Waypoint
_MCG_ =
  VFR_Waypoint
    "MELBOURNE CRICKET GROUND"
    (Just "VIC")
    "MCG"
    (-37.82)
    144.98333333333332

_MELS_ ::
  VFR_Waypoint
_MELS_ =
  VFR_Waypoint
    "MELTON SOUTH"
    (Just "VIC")
    "MELS"
    (-37.68333333333333)
    144.56666666666666

_MEG_ ::
  VFR_Waypoint
_MEG_ =
  VFR_Waypoint
    "MENANGLE"
    (Just "NSW")
    "MEG"
    (-34.125)
    150.74166666666667

_MRI_ ::
  VFR_Waypoint
_MRI_ =
  VFR_Waypoint
    "MERION"
    (Just "QLD")
    "MRI"
    (-22.866666666666667)
    149.03333333333333

_MRJ_ ::
  VFR_Waypoint
_MRJ_ =
  VFR_Waypoint
    "MERRIJIG"
    (Just "VIC")
    "MRJ"
    (-37.11666666666667)
    146.25

_MIAR_ ::
  VFR_Waypoint
_MIAR_ =
  VFR_Waypoint
    "MIAREE POOL BRIDGE"
    (Just "WA")
    "MIAR"
    (-20.85)
    116.61

_MHGO_ ::
  VFR_Waypoint
_MHGO_ =
  VFR_Waypoint
    "MICHELAGO"
    (Just "NSW")
    "MHGO"
    (-35.71666666666667)
    149.16666666666666

_MII_ ::
  VFR_Waypoint
_MII_ =
  VFR_Waypoint
    "MIDGE ISLAND"
    (Just "QLD")
    "MII"
    (-20.691666666666666)
    148.76166666666666

_MCRO_ ::
  VFR_Waypoint
_MCRO_ =
  VFR_Waypoint
    "MILLS CROSS"
    (Just "NSW")
    "MCRO"
    (-35.37166666666667)
    149.425

_MSTM_ ::
  VFR_Waypoint
_MSTM_ =
  VFR_Waypoint
    "MILLSTREAM STN"
    (Just "WA")
    "MSTM"
    (-21.616666666666667)
    117.06666666666666

_MPO_ ::
  VFR_Waypoint
_MPO_ =
  VFR_Waypoint
    "MILPEROO"
    (Just "QLD")
    "MPO"
    (-23.183333333333334)
    141.25

_MIJ_ ::
  VFR_Waypoint
_MIJ_ =
  VFR_Waypoint
    "MINJILANG"
    (Just "NT")
    "MIJ"
    (-11.148333333333333)
    132.58

_MISB_ ::
  VFR_Waypoint
_MISB_ =
  VFR_Waypoint
    "MISSION BEACH"
    (Just "QLD")
    "MISB"
    (-17.87)
    146.10666666666665

_MSC_ ::
  VFR_Waypoint
_MSC_ =
  VFR_Waypoint
    "MISSION BEACH"
    (Just "QLD")
    "MSC"
    (-17.883333333333333)
    146.1

_MITI_ ::
  VFR_Waypoint
_MITI_ =
  VFR_Waypoint
    "MITIAMO"
    (Just "VIC")
    "MITI"
    (-36.211666666666666)
    144.23

_MFBH_ ::
  VFR_Waypoint
_MFBH_ =
  VFR_Waypoint
    "MOFFAT BEACH"
    (Just "QLD")
    "MFBH"
    (-26.788333333333334)
    153.14166666666668

_MFH_ ::
  VFR_Waypoint
_MFH_ =
  VFR_Waypoint
    "MOFFAT HEAD"
    (Just "QLD")
    "MFH"
    (-26.79)
    153.14

_MBH_ ::
  VFR_Waypoint
_MBH_ =
  VFR_Waypoint
    "MOOLOOLABA"
    (Just "QLD")
    "MBH"
    (-26.7)
    153.13333333333333

_MVC_ ::
  VFR_Waypoint
_MVC_ =
  VFR_Waypoint
    "MOONEE VALLEY RACECOURSE"
    (Just "VIC")
    "MVC"
    (-37.766666666666666)
    144.93333333333334

_MIE_ ::
  VFR_Waypoint
_MIE_ =
  VFR_Waypoint
    "MOONIE"
    (Just "QLD")
    "MIE"
    (-27.716666666666665)
    150.36666666666667

_MPSC_ ::
  VFR_Waypoint
_MPSC_ =
  VFR_Waypoint
    "MOORE PARK SUPA CENTRE"
    (Just "NSW")
    "MPSC"
    (-33.903333333333336)
    151.215

_MORN_ ::
  VFR_Waypoint
_MORN_ =
  VFR_Waypoint
    "MORNINGTON"
    (Just "TAS")
    "MORN"
    (-42.85666666666667)
    147.41166666666666

_MSV_ ::
  VFR_Waypoint
_MSV_ =
  VFR_Waypoint
    "MOSS VALE"
    (Just "NSW")
    "MSV"
    (-34.525)
    150.42166666666665

_MMA_ ::
  VFR_Waypoint
_MMA_ =
  VFR_Waypoint
    "MOSSMAN"
    (Just "QLD")
    "MMA"
    (-16.458333333333332)
    145.37166666666667

_BAK_ ::
  VFR_Waypoint
_BAK_ =
  VFR_Waypoint
    "MOUNT BARKER"
    (Just "SA")
    "BAK"
    (-35.07833333333333)
    138.86666666666667

_MBST_ ::
  VFR_Waypoint
_MBST_ =
  VFR_Waypoint
    "MOUNT BENSTEAD"
    (Just "NT")
    "MBST"
    (-23.566666666666666)
    134.275

_MBC_ ::
  VFR_Waypoint
_MBC_ =
  VFR_Waypoint
    "MOUNT BLACK"
    (Just "QLD")
    "MBC"
    (-19.283333333333335)
    146.55833333333334

_MBK_ ::
  VFR_Waypoint
_MBK_ =
  VFR_Waypoint
    "MOUNT BLACKWOOD"
    (Just "QLD")
    "MBK"
    (-21.033333333333335)
    148.94166666666666

_MTBL_ ::
  VFR_Waypoint
_MTBL_ =
  VFR_Waypoint
    "MOUNT BOHLE"
    (Just "QLD")
    "MTBL"
    (-19.266666666666666)
    146.68833333333333

_MBR_ ::
  VFR_Waypoint
_MBR_ =
  VFR_Waypoint
    "MOUNT BOLD RESV"
    (Just "SA")
    "MBR"
    (-35.12166666666667)
    138.7

_MTB_ ::
  VFR_Waypoint
_MTB_ =
  VFR_Waypoint
    "MOUNT BOYCE"
    (Just "NSW")
    "MTB"
    (-33.61833333333333)
    150.27333333333334

_MCAR_ ::
  VFR_Waypoint
_MCAR_ =
  VFR_Waypoint
    "MOUNT CARNARVON"
    (Just "QLD")
    "MCAR"
    (-24.916666666666668)
    148.38333333333333

_MCHR_ ::
  VFR_Waypoint
_MCHR_ =
  VFR_Waypoint
    "MOUNT CHRISTIE"
    (Just "SA")
    "MCHR"
    (-30.55)
    133.21666666666667

_MCOM_ ::
  VFR_Waypoint
_MCOM_ =
  VFR_Waypoint
    "MOUNT COMPASS"
    (Just "SA")
    "MCOM"
    (-35.35)
    138.61666666666667

_MTK_ ::
  VFR_Waypoint
_MTK_ =
  VFR_Waypoint
    "MOUNT COOKE"
    (Just "WA")
    "MTK"
    (-32.416666666666664)
    116.30666666666667

_MTC_ ::
  VFR_Waypoint
_MTC_ =
  VFR_Waypoint
    "MOUNT COOLUM"
    (Just "QLD")
    "MTC"
    (-26.561666666666667)
    153.08333333333334

_MTBA_ ::
  VFR_Waypoint
_MTBA_ =
  VFR_Waypoint
    "MOUNT CORAMBA"
    (Just "NSW")
    "MTBA"
    (-30.221666666666668)
    153.05

_TCR_ ::
  VFR_Waypoint
_TCR_ =
  VFR_Waypoint
    "MOUNT COREE"
    (Just "ACT")
    "TCR"
    (-35.30833333333333)
    148.81

_MCOO_ ::
  VFR_Waypoint
_MCOO_ =
  VFR_Waypoint
    "MOUNT COTTON"
    (Just "QLD")
    "MCOO"
    (-27.621666666666666)
    153.21666666666667

_MCOT_ ::
  VFR_Waypoint
_MCOT_ =
  VFR_Waypoint
    "MOUNT COTTRELL"
    (Just "VIC")
    "MCOT"
    (-37.763333333333335)
    144.62

_MUE_ ::
  VFR_Waypoint
_MUE_ =
  VFR_Waypoint
    "MOUNT DALE"
    (Just "WA")
    "MUE"
    (-32.126666666666665)
    116.29666666666667

_MDY_ ::
  VFR_Waypoint
_MDY_ =
  VFR_Waypoint
    "MOUNT DAY"
    (Just "WA")
    "MDY"
    (-32.13333333333333)
    120.5

_MELE_ ::
  VFR_Waypoint
_MELE_ =
  VFR_Waypoint
    "MOUNT ELEPHANT"
    (Just "QLD")
    "MELE"
    (-16.45)
    144.93333333333334

_MEV_ ::
  VFR_Waypoint
_MEV_ =
  VFR_Waypoint
    "MOUNT EVERARD"
    (Just "WA")
    "MEV"
    (-25.183333333333334)
    125.06666666666666

_MFN_ ::
  VFR_Waypoint
_MFN_ =
  VFR_Waypoint
    "MOUNT FUNNEL"
    (Just "QLD")
    "MFN"
    (-21.625)
    149.38666666666666

_MGLO_ ::
  VFR_Waypoint
_MGLO_ =
  VFR_Waypoint
    "MOUNT GLORIOUS"
    (Just "QLD")
    "MGLO"
    (-27.333333333333332)
    152.76666666666668

_MVT_ ::
  VFR_Waypoint
_MVT_ =
  VFR_Waypoint
    "MOUNT GRAVATT"
    (Just "QLD")
    "MVT"
    (-27.55)
    153.075

_MJK_ ::
  VFR_Waypoint
_MJK_ =
  VFR_Waypoint
    "MOUNT JACKSON HS"
    (Just "WA")
    "MJK"
    (-30.2)
    119.1

_MTKI_ ::
  VFR_Waypoint
_MTKI_ =
  VFR_Waypoint
    "MOUNT KINGSTON"
    (Just "NT")
    "MTKI"
    (-25.433333333333334)
    133.63333333333333

_MLI_ ::
  VFR_Waypoint
_MLI_ =
  VFR_Waypoint
    "MOUNT LION"
    (Just "QLD")
    "MLI"
    (-23.391666666666666)
    150.32166666666666

_MLUY_ ::
  VFR_Waypoint
_MLUY_ =
  VFR_Waypoint
    "MOUNT LUCY"
    (Just "NT")
    "MLUY"
    (-22.6)
    133.53333333333333

_MCD_ ::
  VFR_Waypoint
_MCD_ =
  VFR_Waypoint
    "MOUNT MACEDON"
    (Just "VIC")
    "MCD"
    (-37.375)
    144.57666666666665

_MTMA_ ::
  VFR_Waypoint
_MTMA_ =
  VFR_Waypoint
    "MOUNT MARIA"
    (Just "QLD")
    "MTMA"
    (-27.466666666666665)
    151.48333333333332

_MMY_ ::
  VFR_Waypoint
_MMY_ =
  VFR_Waypoint
    "MOUNT MOLLOY"
    (Just "QLD")
    "MMY"
    (-16.678333333333335)
    145.33

_MOB_ ::
  VFR_Waypoint
_MOB_ =
  VFR_Waypoint
    "MOUNT MOOMBIL"
    (Just "NSW")
    "MOB"
    (-30.315)
    152.85166666666666

_MGN_ ::
  VFR_Waypoint
_MGN_ =
  VFR_Waypoint
    "MOUNT MORGAN"
    (Just "QLD")
    "MGN"
    (-23.645)
    150.39166666666668

_MTM_ ::
  VFR_Waypoint
_MTM_ =
  VFR_Waypoint
    "MOUNT MUGGA"
    (Just "ACT")
    "MTM"
    (-35.355)
    149.13

_MIY_ ::
  VFR_Waypoint
_MIY_ =
  VFR_Waypoint
    "MOUNT NINDERRY"
    (Just "QLD")
    "MIY"
    (-26.555)
    152.99166666666667

_OOR_ ::
  VFR_Waypoint
_OOR_ =
  VFR_Waypoint
    "MOUNT OORAMINNA"
    (Just "NT")
    "OOR"
    (-24.091666666666665)
    134.00333333333333

_MPG_ ::
  VFR_Waypoint
_MPG_ =
  VFR_Waypoint
    "MOUNT PALERANG"
    (Just "NSW")
    "MPG"
    (-35.43333333333333)
    149.6

_PIPR_ ::
  VFR_Waypoint
_PIPR_ =
  VFR_Waypoint
    "MOUNT PIPER"
    (Just "VIC")
    "PIPR"
    (-37.205)
    145.00333333333333

_MPT_ ::
  VFR_Waypoint
_MPT_ =
  VFR_Waypoint
    "MOUNT PLEASANT"
    (Just "SA")
    "MPT"
    (-34.775)
    139.05

_SOV_ ::
  VFR_Waypoint
_SOV_ =
  VFR_Waypoint
    "MOUNT SOMERVILLE RADAR"
    (Just "NSW")
    "SOV"
    (-28.215)
    153.42666666666668

_MTEW_ ::
  VFR_Waypoint
_MTEW_ =
  VFR_Waypoint
    "MOUNT STEWAN"
    (Just "QLD")
    "MTEW"
    (-20.366666666666667)
    144.05

_MUM_ ::
  VFR_Waypoint
_MUM_ =
  VFR_Waypoint
    "MOUNT STROMLO"
    (Just "ACT")
    "MUM"
    (-35.31666666666667)
    149.00833333333333

_MUSD_ ::
  VFR_Waypoint
_MUSD_ =
  VFR_Waypoint
    "MOUNT SYDNEY"
    (Just "WA")
    "MUSD"
    (-21.4)
    121.2

_MTLR_ ::
  VFR_Waypoint
_MTLR_ =
  VFR_Waypoint
    "MOUNT TAYLOR"
    (Just "ACT")
    "MTLR"
    (-35.373333333333335)
    149.07666666666665

_MTY_ ::
  VFR_Waypoint
_MTY_ =
  VFR_Waypoint
    "MOUNT TYSON"
    (Just "QLD")
    "MTY"
    (-27.583333333333332)
    151.56666666666666

_MUO_ ::
  VFR_Waypoint
_MUO_ =
  VFR_Waypoint
    "MOUNT UNDOOLYA"
    (Just "NT")
    "MUO"
    (-23.738333333333333)
    134.10333333333332

_MVO_ ::
  VFR_Waypoint
_MVO_ =
  VFR_Waypoint
    "MOUNT VERNON HS"
    (Just "WA")
    "MVO"
    (-24.233333333333334)
    118.23333333333333

_MVI_ ::
  VFR_Waypoint
_MVI_ =
  VFR_Waypoint
    "MOUNT VICTORIA"
    (Just "NSW")
    "MVI"
    (-33.583333333333336)
    150.25

_MTWK_ ::
  VFR_Waypoint
_MTWK_ =
  VFR_Waypoint
    "MOUNT WALKER"
    (Just "QLD")
    "MTWK"
    (-27.788333333333334)
    152.55666666666667

_MTWG_ ::
  VFR_Waypoint
_MTWG_ =
  VFR_Waypoint
    "MOUNT WARNING"
    (Just "NSW")
    "MTWG"
    (-28.4)
    153.26666666666668

_MTWN_ ::
  VFR_Waypoint
_MTWN_ =
  VFR_Waypoint
    "MOUNT WELLINGTON"
    (Just "VIC")
    "MTWN"
    (-37.56)
    146.81

_MWH_ ::
  VFR_Waypoint
_MWH_ =
  VFR_Waypoint
    "MOUNT WHEELER"
    (Just "QLD")
    "MWH"
    (-23.226666666666667)
    150.68333333333334

_MWK_ ::
  VFR_Waypoint
_MWK_ =
  VFR_Waypoint
    "MOUNT WILKIE"
    (Just "WA")
    "MWK"
    (-20.951666666666668)
    116.41833333333334

_MBKR_ ::
  VFR_Waypoint
_MBKR_ =
  VFR_Waypoint
    "MOUTH OF THE BLACK RIVER"
    (Just "QLD")
    "MBKR"
    (-19.18)
    146.65333333333334

_MBHR_ ::
  VFR_Waypoint
_MBHR_ =
  VFR_Waypoint
    "MOUTH OF THE BOHLE RIVER"
    (Just "QLD")
    "MBHR"
    (-19.196666666666665)
    146.70166666666665

_MMT_ ::
  VFR_Waypoint
_MMT_ =
  VFR_Waypoint
    "MT MARGARET"
    (Just "QLD")
    "MMT"
    (-19.35)
    146.60166666666666

_MUDI_ ::
  VFR_Waypoint
_MUDI_ =
  VFR_Waypoint
    "MUD ISLAND"
    (Just "QLD")
    "MUDI"
    (-27.333333333333332)
    153.25

_MEER_ ::
  VFR_Waypoint
_MEER_ =
  VFR_Waypoint
    "MUDGEERABA"
    (Just "QLD")
    "MEER"
    (-28.083333333333332)
    153.36666666666667

_MUP_ ::
  VFR_Waypoint
_MUP_ =
  VFR_Waypoint
    "MULLALOO PT"
    (Just "WA")
    "MUP"
    (-31.808333333333334)
    115.725

_LLN_ ::
  VFR_Waypoint
_LLN_ =
  VFR_Waypoint
    "MULLEN"
    (Just "QLD")
    "LLN"
    (-25.036666666666665)
    153.0

_MBBY_ ::
  VFR_Waypoint
_MBBY_ =
  VFR_Waypoint
    "MULLUMBIMBY"
    (Just "NSW")
    "MBBY"
    (-28.55)
    153.5

_MWR_ ::
  VFR_Waypoint
_MWR_ =
  VFR_Waypoint
    "MUNDARING WEIR"
    (Just "WA")
    "MWR"
    (-31.955)
    116.15833333333333

_MAA_ ::
  VFR_Waypoint
_MAA_ =
  VFR_Waypoint
    "MUNGALLALA"
    (Just "QLD")
    "MAA"
    (-26.45)
    147.55

_MUNM_ ::
  VFR_Waypoint
_MUNM_ =
  VFR_Waypoint
    "MUNIGANEEN MT"
    (Just "QLD")
    "MUNM"
    (-27.408333333333335)
    151.875

_MHY_ ::
  VFR_Waypoint
_MHY_ =
  VFR_Waypoint
    "MURPHY'S CREEK"
    (Just "QLD")
    "MHY"
    (-27.463333333333335)
    152.055

_MBD_ ::
  VFR_Waypoint
_MBD_ =
  VFR_Waypoint
    "MURRAY BRIDGE"
    (Just "SA")
    "MBD"
    (-35.15)
    139.31

_MYW_ ::
  VFR_Waypoint
_MYW_ =
  VFR_Waypoint
    "MURRAY DOWNS"
    (Just "QLD")
    "MYW"
    (-25.033333333333335)
    139.2

_MMM_ ::
  VFR_Waypoint
_MMM_ =
  VFR_Waypoint
    "MURRUMBATEMAN"
    (Just "NSW")
    "MMM"
    (-34.971666666666664)
    149.025

_MUI_ ::
  VFR_Waypoint
_MUI_ =
  VFR_Waypoint
    "MURRURUNDI"
    (Just "NSW")
    "MUI"
    (-31.766666666666666)
    150.83333333333334

_MUR_ ::
  VFR_Waypoint
_MUR_ =
  VFR_Waypoint
    "MURWILLUMBAH"
    (Just "NSW")
    "MUR"
    (-28.325)
    153.39666666666668

_MUEE_ ::
  VFR_Waypoint
_MUEE_ =
  VFR_Waypoint
    "MUTARNEE"
    (Just "QLD")
    "MUEE"
    (-18.95)
    146.3

_MRTL_ ::
  VFR_Waypoint
_MRTL_ =
  VFR_Waypoint
    "MYRTLE"
    (Just "QLD")
    "MRTL"
    (-19.7)
    146.53333333333333

_NMB_ ::
  VFR_Waypoint
_NMB_ =
  VFR_Waypoint
    "NAMBOUR"
    (Just "QLD")
    "NMB"
    (-26.628333333333334)
    152.95833333333334

_NHS_ ::
  VFR_Waypoint
_NHS_ =
  VFR_Waypoint
    "NAMBUCCA HEADS"
    (Just "NSW")
    "NHS"
    (-30.645)
    153.00833333333333

_NAA_ ::
  VFR_Waypoint
_NAA_ =
  VFR_Waypoint
    "NARA INLET"
    (Just "QLD")
    "NAA"
    (-20.15)
    148.9

_NAMA_ ::
  VFR_Waypoint
_NAMA_ =
  VFR_Waypoint
    "NAROOMA"
    (Just "NSW")
    "NAMA"
    (-36.2)
    150.13333333333333

_NRW_ ::
  VFR_Waypoint
_NRW_ =
  VFR_Waypoint
    "NARRE WARREN"
    (Just "VIC")
    "NRW"
    (-38.016666666666666)
    145.3

_NOOG_ ::
  VFR_Waypoint
_NOOG_ =
  VFR_Waypoint
    "NARROOGAL"
    (Just "QLD")
    "NOOG"
    (-10.25)
    142.5

_NKBO_ ::
  VFR_Waypoint
_NKBO_ =
  VFR_Waypoint
    "NECKARBOO"
    (Just "NSW")
    "NKBO"
    (-32.06666666666667)
    144.61666666666667

_NEM_ ::
  VFR_Waypoint
_NEM_ =
  VFR_Waypoint
    "NEMINGHA"
    (Just "NSW")
    "NEM"
    (-31.125)
    150.99166666666667

_NPBR_ ::
  VFR_Waypoint
_NPBR_ =
  VFR_Waypoint
    "NEPEAN BRIDGE"
    (Just "NSW")
    "NPBR"
    (-33.763333333333335)
    150.66

_NEN_ ::
  VFR_Waypoint
_NEN_ =
  VFR_Waypoint
    "NERANG"
    (Just "QLD")
    "NEN"
    (-27.988333333333333)
    153.33833333333334

_NGI_ ::
  VFR_Waypoint
_NGI_ =
  VFR_Waypoint
    "NGULUPI"
    (Just "QLD")
    "NGI"
    (-10.243333333333334)
    142.41

_NTT_ ::
  VFR_Waypoint
_NTT_ =
  VFR_Waypoint
    "NIMBIN TV TOWERS"
    (Just "NSW")
    "NTT"
    (-28.541666666666668)
    153.29166666666666

_NIM_ ::
  VFR_Waypoint
_NIM_ =
  VFR_Waypoint
    "NIMROD PASSAGE"
    (Just "QLD")
    "NIM"
    (-12.1)
    143.78333333333333

_NDI_ ::
  VFR_Waypoint
_NDI_ =
  VFR_Waypoint
    "NINDIGULLY"
    (Just "QLD")
    "NDI"
    (-28.35)
    148.81666666666666

_NARL_ ::
  VFR_Waypoint
_NARL_ =
  VFR_Waypoint
    "NOARLUNGA"
    (Just "SA")
    "NARL"
    (-35.141666666666666)
    138.48833333333334

_NBB_ ::
  VFR_Waypoint
_NBB_ =
  VFR_Waypoint
    "NOBBYS HEAD"
    (Just "NSW")
    "NBB"
    (-32.915)
    151.79

_NDY_ ::
  VFR_Waypoint
_NDY_ =
  VFR_Waypoint
    "NODDY REEF"
    (Just "QLD")
    "NDY"
    (-13.733333333333333)
    143.75

_NOME_ ::
  VFR_Waypoint
_NOME_ =
  VFR_Waypoint
    "NOME"
    (Just "QLD")
    "NOME"
    (-19.376666666666665)
    146.92

_NNDO_ ::
  VFR_Waypoint
_NNDO_ =
  VFR_Waypoint
    "NOONDOO"
    (Just "QLD")
    "NNDO"
    (-28.616666666666667)
    148.43333333333334

_NOSA_ ::
  VFR_Waypoint
_NOSA_ =
  VFR_Waypoint
    "NOOSA HEADS"
    (Just "QLD")
    "NOSA"
    (-26.375)
    153.11666666666667

_NEQ_ ::
  VFR_Waypoint
_NEQ_ =
  VFR_Waypoint
    "NORTH EAST QUARRY"
    (Just "VIC")
    "NEQ"
    (-37.94166666666667)
    144.58333333333334

_NOHD_ ::
  VFR_Waypoint
_NOHD_ =
  VFR_Waypoint
    "NORTH HEAD"
    (Just "NSW")
    "NOHD"
    (-33.821666666666665)
    151.29166666666666

_NORT_ ::
  VFR_Waypoint
_NORT_ =
  VFR_Waypoint
    "NORTH LAKE"
    (Just "WA")
    "NORT"
    (-32.07666666666667)
    115.82333333333334

_NSTA_ ::
  VFR_Waypoint
_NSTA_ =
  VFR_Waypoint
    "NORTH STAR"
    (Just "NSW")
    "NSTA"
    (-28.916666666666668)
    150.41666666666666

_NBRR_ ::
  VFR_Waypoint
_NBRR_ =
  VFR_Waypoint
    "NORTHERN TIP BERSERKERS"
    (Just "QLD")
    "NBRR"
    (-23.278333333333332)
    150.58833333333334

_NRWN_ ::
  VFR_Waypoint
_NRWN_ =
  VFR_Waypoint
    "NORWIN"
    (Just "QLD")
    "NRWN"
    (-27.558333333333334)
    151.38

_NUDG_ ::
  VFR_Waypoint
_NUDG_ =
  VFR_Waypoint
    "NUDGEE TIP"
    (Just "QLD")
    "NUDG"
    (-27.358333333333334)
    153.09166666666667

_NUA_ ::
  VFR_Waypoint
_NUA_ =
  VFR_Waypoint
    "NUNAMARA"
    (Just "TAS")
    "NUA"
    (-41.391666666666666)
    147.3

_NUN_ ::
  VFR_Waypoint
_NUN_ =
  VFR_Waypoint
    "NUNDLE"
    (Just "NSW")
    "NUN"
    (-31.466666666666665)
    151.125

_NUPA_ ::
  VFR_Waypoint
_NUPA_ =
  VFR_Waypoint
    "NURIOOTPA"
    (Just "SA")
    "NUPA"
    (-34.483333333333334)
    139.0

_NCHU_ ::
  VFR_Waypoint
_NCHU_ =
  VFR_Waypoint
    "NYCHUM"
    (Just "QLD")
    "NCHU"
    (-16.843333333333334)
    144.46166666666667

_OAT_ ::
  VFR_Waypoint
_OAT_ =
  VFR_Waypoint
    "OATLANDS"
    (Just "TAS")
    "OAT"
    (-42.3)
    147.36666666666667

_OBC_ ::
  VFR_Waypoint
_OBC_ =
  VFR_Waypoint
    "OBSERVATION CITY"
    (Just "WA")
    "OBC"
    (-31.895)
    115.755

_OBSH_ ::
  VFR_Waypoint
_OBSH_ =
  VFR_Waypoint
    "OBSERVATORY HILL"
    (Just "SA")
    "OBSH"
    (-28.966666666666665)
    132.0

_OBY_ ::
  VFR_Waypoint
_OBY_ =
  VFR_Waypoint
    "OLD BOMBANDY"
    (Just "QLD")
    "OBY"
    (-22.433333333333334)
    148.63333333333333

_OLCO_ ::
  VFR_Waypoint
_OLCO_ =
  VFR_Waypoint
    "OLD CORK"
    (Just "QLD")
    "OLCO"
    (-22.933333333333334)
    141.86666666666667

_OLSOD_ ::
  VFR_Waypoint
_OLSOD_ =
  VFR_Waypoint
    "OLSOD"
    Nothing
    "OLSOD"
    (-8.54)
    144.45333333333335

_ONPK_ ::
  VFR_Waypoint
_ONPK_ =
  VFR_Waypoint
    "ORAN PARK"
    (Just "NSW")
    "ONPK"
    (-34.00833333333333)
    150.74166666666667

_ORKS_ ::
  VFR_Waypoint
_ORKS_ =
  VFR_Waypoint
    "ORCHARD ROCKS"
    (Just "QLD")
    "ORKS"
    (-19.11)
    146.88166666666666

_ORF_ ::
  VFR_Waypoint
_ORF_ =
  VFR_Waypoint
    "ORFORD NESS"
    (Just "QLD")
    "ORF"
    (-11.3)
    142.81666666666666

_OHB_ ::
  VFR_Waypoint
_OHB_ =
  VFR_Waypoint
    "OUTER HARBOR"
    (Just "SA")
    "OHB"
    (-34.775)
    138.48333333333332

_OVL_ ::
  VFR_Waypoint
_OVL_ =
  VFR_Waypoint
    "OVERLANDER"
    (Just "WA")
    "OVL"
    (-26.4)
    114.46666666666667

_OEN_ ::
  VFR_Waypoint
_OEN_ =
  VFR_Waypoint
    "OWEN"
    (Just "SA")
    "OEN"
    (-34.27)
    138.54166666666666

_OWS_ ::
  VFR_Waypoint
_OWS_ =
  VFR_Waypoint
    "OWEN SPRINGS"
    (Just "NT")
    "OWS"
    (-23.875)
    133.47166666666666

_OFD_ ::
  VFR_Waypoint
_OFD_ =
  VFR_Waypoint
    "OXENFORD"
    (Just "QLD")
    "OFD"
    (-27.883333333333333)
    153.31666666666666

_PCVE_ ::
  VFR_Waypoint
_PCVE_ =
  VFR_Waypoint
    "PALM COVE"
    (Just "QLD")
    "PCVE"
    (-16.75)
    145.66666666666666

_PFRM_ ::
  VFR_Waypoint
_PFRM_ =
  VFR_Waypoint
    "PALM FARM"
    (Just "QLD")
    "PFRM"
    (-17.043333333333333)
    145.76166666666666

_PLW_ ::
  VFR_Waypoint
_PLW_ =
  VFR_Waypoint
    "PALM MEADOWS"
    (Just "QLD")
    "PLW"
    (-28.033333333333335)
    152.40833333333333

_PLU_ ::
  VFR_Waypoint
_PLU_ =
  VFR_Waypoint
    "PALUMA DAM"
    (Just "QLD")
    "PLU"
    (-18.955)
    146.145

_SFG_ ::
  VFR_Waypoint
_SFG_ =
  VFR_Waypoint
    "PARADISE GARDENS"
    (Just "QLD")
    "SFG"
    (-28.018333333333334)
    153.375

_PKR_ ::
  VFR_Waypoint
_PKR_ =
  VFR_Waypoint
    "PARK RIDGE WATER TOWER"
    (Just "QLD")
    "PKR"
    (-27.705)
    153.03833333333333

_PRKH_ ::
  VFR_Waypoint
_PRKH_ =
  VFR_Waypoint
    "PARKHURST"
    (Just "QLD")
    "PRKH"
    (-23.303333333333335)
    150.51333333333332

_PRT_ ::
  VFR_Waypoint
_PRT_ =
  VFR_Waypoint
    "PARRAMATTA"
    (Just "NSW")
    "PRT"
    (-33.81666666666667)
    151.005

_PAA_ ::
  VFR_Waypoint
_PAA_ =
  VFR_Waypoint
    "PATONGA"
    (Just "NSW")
    "PAA"
    (-33.55166666666667)
    151.26333333333332

_PECO_ ::
  VFR_Waypoint
_PECO_ =
  VFR_Waypoint
    "PEACOCK"
    (Just "QLD")
    "PECO"
    (-18.683333333333334)
    145.98333333333332

_PDNE_ ::
  VFR_Waypoint
_PDNE_ =
  VFR_Waypoint
    "PEAK DOWNS MINE"
    (Just "QLD")
    "PDNE"
    (-22.25)
    148.18333333333334

_PEAR_ ::
  VFR_Waypoint
_PEAR_ =
  VFR_Waypoint
    "PEARSON ISLES"
    (Just "SA")
    "PEAR"
    (-33.95)
    134.26666666666668

_PCCK_ ::
  VFR_Waypoint
_PCCK_ =
  VFR_Waypoint
    "PELICAN CREEK"
    (Just "QLD")
    "PCCK"
    (-25.233333333333334)
    150.9

_PENH_ ::
  VFR_Waypoint
_PENH_ =
  VFR_Waypoint
    "PENNANT HILLS STROBE"
    (Just "NSW")
    "PENH"
    (-33.74)
    151.07

_PENT_ ::
  VFR_Waypoint
_PENT_ =
  VFR_Waypoint
    "PENRITH"
    (Just "NSW")
    "PENT"
    (-33.75833333333333)
    150.7

_PVS_ ::
  VFR_Waypoint
_PVS_ =
  VFR_Waypoint
    "PERCIVAL LAKES"
    (Just "WA")
    "PVS"
    (-21.58)
    124.16

_PEG_ ::
  VFR_Waypoint
_PEG_ =
  VFR_Waypoint
    "PEREGIAN"
    (Just "QLD")
    "PEG"
    (-26.516666666666666)
    153.1

_PCKD_ ::
  VFR_Waypoint
_PCKD_ =
  VFR_Waypoint
    "PERSEVERENCE CREEK DAM"
    (Just "QLD")
    "PCKD"
    (-27.305)
    152.12166666666667

_PCTY_ ::
  VFR_Waypoint
_PCTY_ =
  VFR_Waypoint
    "PERTH CITY"
    (Just "WA")
    "PCTY"
    (-31.955)
    115.85666666666667

_PTI_ ::
  VFR_Waypoint
_PTI_ =
  VFR_Waypoint
    "PETRIE"
    (Just "QLD")
    "PTI"
    (-27.266666666666666)
    152.975

_PIB_ ::
  VFR_Waypoint
_PIB_ =
  VFR_Waypoint
    "PICKERING BROOK GOLF COURSE"
    (Just "WA")
    "PIB"
    (-32.038333333333334)
    116.11166666666666

_PIL_ ::
  VFR_Waypoint
_PIL_ =
  VFR_Waypoint
    "PICKET HILL"
    (Just "NSW")
    "PIL"
    (-30.571666666666665)
    152.98333333333332

_PCA_ ::
  VFR_Waypoint
_PCA_ =
  VFR_Waypoint
    "PICNIC BAY"
    (Just "QLD")
    "PCA"
    (-19.183333333333334)
    146.85

_PNP_ ::
  VFR_Waypoint
_PNP_ =
  VFR_Waypoint
    "PICNIC PT"
    (Just "NSW")
    "PNP"
    (-33.98)
    151.00166666666667

_PIC_ ::
  VFR_Waypoint
_PIC_ =
  VFR_Waypoint
    "PICTON"
    (Just "NSW")
    "PIC"
    (-34.175)
    150.61666666666667

_PIG_ ::
  VFR_Waypoint
_PIG_ =
  VFR_Waypoint
    "PIGGERY"
    (Just "NSW")
    "PIG"
    (-36.013333333333335)
    146.79166666666666

_PIPT_ ::
  VFR_Waypoint
_PIPT_ =
  VFR_Waypoint
    "PINE PT"
    (Just "SA")
    "PIPT"
    (-34.56666666666667)
    137.88333333333333

_PING_ ::
  VFR_Waypoint
_PING_ =
  VFR_Waypoint
    "PINGARING"
    (Just "WA")
    "PING"
    (-32.75833333333333)
    118.625

_PII_ ::
  VFR_Waypoint
_PII_ =
  VFR_Waypoint
    "PIRATE PT"
    (Just "QLD")
    "PII"
    (-23.508333333333333)
    150.64166666666668

_PWH_ ::
  VFR_Waypoint
_PWH_ =
  VFR_Waypoint
    "PITTSWORTH"
    (Just "QLD")
    "PWH"
    (-27.721666666666668)
    151.63333333333333

_PTOM_ ::
  VFR_Waypoint
_PTOM_ =
  VFR_Waypoint
    "POINT ORMOND"
    (Just "VIC")
    "PTOM"
    (-37.88333333333333)
    144.98333333333332

_PSS_ ::
  VFR_Waypoint
_PSS_ =
  VFR_Waypoint
    "POINT STEPHENS LIGHTHOUSE"
    (Just "NSW")
    "PSS"
    (-32.74666666666667)
    152.20833333333334

_PRP_ ::
  VFR_Waypoint
_PRP_ =
  VFR_Waypoint
    "PORPOISE PT"
    (Just "QLD")
    "PRP"
    (-27.936666666666667)
    153.425

_PAL_ ::
  VFR_Waypoint
_PAL_ =
  VFR_Waypoint
    "PORT ADELAIDE"
    (Just "SA")
    "PAL"
    (-34.85)
    138.5

_PMA_ ::
  VFR_Waypoint
_PMA_ =
  VFR_Waypoint
    "PORT ALMA"
    (Just "QLD")
    "PMA"
    (-23.583333333333332)
    150.85833333333332

_POMP_ ::
  VFR_Waypoint
_POMP_ =
  VFR_Waypoint
    "PORT CAMPBELL"
    (Just "VIC")
    "POMP"
    (-38.62)
    142.995

_PDV_ ::
  VFR_Waypoint
_PDV_ =
  VFR_Waypoint
    "PORT DAVEY"
    (Just "TAS")
    "PDV"
    (-43.333333333333336)
    145.88333333333333

_PTD_ ::
  VFR_Waypoint
_PTD_ =
  VFR_Waypoint
    "PORT DOUGLAS"
    (Just "QLD")
    "PTD"
    (-16.483333333333334)
    145.46333333333334

_PJUL_ ::
  VFR_Waypoint
_PJUL_ =
  VFR_Waypoint
    "PORT JULIA"
    (Just "SA")
    "PJUL"
    (-34.666666666666664)
    137.87833333333333

_PMG_ ::
  VFR_Waypoint
_PMG_ =
  VFR_Waypoint
    "PORT MUSGRAVE"
    (Just "QLD")
    "PMG"
    (-12.0)
    141.93333333333334

_PNE_ ::
  VFR_Waypoint
_PNE_ =
  VFR_Waypoint
    "PORT NEILL"
    (Just "SA")
    "PNE"
    (-34.11666666666667)
    136.35

_PNL_ ::
  VFR_Waypoint
_PNL_ =
  VFR_Waypoint
    "PORT NOARLUNGA"
    (Just "SA")
    "PNL"
    (-35.15)
    138.46666666666667

_PIPS_ ::
  VFR_Waypoint
_PIPS_ =
  VFR_Waypoint
    "PORT PHILLIP HEADS"
    (Just "VIC")
    "PIPS"
    (-38.29333333333334)
    144.63333333333333

_PVCT_ ::
  VFR_Waypoint
_PVCT_ =
  VFR_Waypoint
    "PORT VINCENT"
    (Just "SA")
    "PVCT"
    (-34.781666666666666)
    137.86166666666668

_POWR_ ::
  VFR_Waypoint
_POWR_ =
  VFR_Waypoint
    "POWERHOUSE"
    (Just "WA")
    "POWR"
    (-32.095)
    115.75666666666666

_PWLC_ ::
  VFR_Waypoint
_PWLC_ =
  VFR_Waypoint
    "POWERLINE CROSSING"
    (Just "VIC")
    "PWLC"
    (-37.61333333333333)
    144.77833333333334

_PRES_ ::
  VFR_Waypoint
_PRES_ =
  VFR_Waypoint
    "PRESCOTT LAKES"
    (Just "WA")
    "PRES"
    (-20.75)
    125.16666666666667

_PRS_ ::
  VFR_Waypoint
_PRS_ =
  VFR_Waypoint
    "PRIMROSE SANDS"
    (Just "TAS")
    "PRS"
    (-42.88333333333333)
    147.66666666666666

_PCB_ ::
  VFR_Waypoint
_PCB_ =
  VFR_Waypoint
    "PRINCESS CHARLOTTE BAY"
    (Just "QLD")
    "PCB"
    (-14.333333333333334)
    144.11666666666667

_PSP_ ::
  VFR_Waypoint
_PSP_ =
  VFR_Waypoint
    "PROSPECT RESV"
    (Just "NSW")
    "PSP"
    (-33.81666666666667)
    150.91666666666666

_PSF_ ::
  VFR_Waypoint
_PSF_ =
  VFR_Waypoint
    "PROSSERS SUGARLOAF"
    (Just "TAS")
    "PSF"
    (-42.675)
    147.825

_PSTO_ ::
  VFR_Waypoint
_PSTO_ =
  VFR_Waypoint
    "PROSTON"
    (Just "QLD")
    "PSTO"
    (-26.166666666666668)
    151.6

_DNGR_ ::
  VFR_Waypoint
_DNGR_ =
  VFR_Waypoint
    "PT DANGER"
    (Just "NSW")
    "DNGR"
    (-28.165)
    153.55166666666668

_FAW_ ::
  VFR_Waypoint
_FAW_ =
  VFR_Waypoint
    "PT FAWCETT"
    (Just "NT")
    "FAW"
    (-11.8)
    130.01666666666668

_PMPH_ ::
  VFR_Waypoint
_PMPH_ =
  VFR_Waypoint
    "PUMPHREY'S BRIDGE"
    (Just "WA")
    "PMPH"
    (-32.666666666666664)
    116.9

_PBF_ ::
  VFR_Waypoint
_PBF_ =
  VFR_Waypoint
    "PURLINGBROOKE FALLS"
    (Just "QLD")
    "PBF"
    (-28.166666666666668)
    153.26666666666668

_PWDA_ ::
  VFR_Waypoint
_PWDA_ =
  VFR_Waypoint
    "PURRAWUNDA"
    (Just "QLD")
    "PWDA"
    (-27.538333333333334)
    151.625

_PUTY_ ::
  VFR_Waypoint
_PUTY_ =
  VFR_Waypoint
    "PUTTY"
    (Just "NSW")
    "PUTY"
    (-32.96666666666667)
    150.75

_PYA_ ::
  VFR_Waypoint
_PYA_ =
  VFR_Waypoint
    "PYALONG"
    (Just "VIC")
    "PYA"
    (-37.11666666666667)
    144.855

_PYK_ ::
  VFR_Waypoint
_PYK_ =
  VFR_Waypoint
    "PYKES CREEK RESV"
    (Just "VIC")
    "PYK"
    (-37.6)
    144.295

_QE2_ ::
  VFR_Waypoint
_QE2_ =
  VFR_Waypoint
    "QE2 STADIUM"
    (Just "QLD")
    "QE2"
    (-27.558333333333334)
    153.06333333333333

_QAI_ ::
  VFR_Waypoint
_QAI_ =
  VFR_Waypoint
    "QUAIL ISLAND"
    (Just "QLD")
    "QAI"
    (-22.133333333333333)
    150.0

_QBN_ ::
  VFR_Waypoint
_QBN_ =
  VFR_Waypoint
    "QUEANBEYAN"
    (Just "NSW")
    "QBN"
    (-35.36)
    149.24333333333334

_Q1_ ::
  VFR_Waypoint
_Q1_ =
  VFR_Waypoint
    "QUEBEC ONE"
    (Just "QLD")
    "Q1"
    (-28.006666666666668)
    153.43

_QLW_ ::
  VFR_Waypoint
_QLW_ =
  VFR_Waypoint
    "QUINALOW"
    (Just "QLD")
    "QLW"
    (-27.105)
    151.62166666666667

_QNDI_ ::
  VFR_Waypoint
_QNDI_ =
  VFR_Waypoint
    "QUIRINDI"
    (Just "NSW")
    "QNDI"
    (-31.49)
    150.51333333333332

_RIL_ ::
  VFR_Waypoint
_RIL_ =
  VFR_Waypoint
    "RABBIT ISLAND"
    (Just "QLD")
    "RIL"
    (-20.833333333333332)
    148.9

_RBY_ ::
  VFR_Waypoint
_RBY_ =
  VFR_Waypoint
    "RABY BAY"
    (Just "QLD")
    "RBY"
    (-27.516666666666666)
    153.275

_RDRS_ ::
  VFR_Waypoint
_RDRS_ =
  VFR_Waypoint
    "RADAR SITE"
    (Just "QLD")
    "RDRS"
    (-19.2)
    146.76666666666668

_RADT_ ::
  VFR_Waypoint
_RADT_ =
  VFR_Waypoint
    "RADIO TELESCOPE"
    (Just "TAS")
    "RADT"
    (-42.81666666666667)
    147.45

_RAIS_ ::
  VFR_Waypoint
_RAIS_ =
  VFR_Waypoint
    "RAINE ISLAND"
    (Just "QLD")
    "RAIS"
    (-11.6)
    144.03333333333333

_RNN_ ::
  VFR_Waypoint
_RNN_ =
  VFR_Waypoint
    "RANNES"
    (Just "QLD")
    "RNN"
    (-24.1)
    150.11666666666667

_RAPD_ ::
  VFR_Waypoint
_RAPD_ =
  VFR_Waypoint
    "RAPID BAY"
    (Just "SA")
    "RAPD"
    (-35.52166666666667)
    138.18333333333334

_RTY_ ::
  VFR_Waypoint
_RTY_ =
  VFR_Waypoint
    "RATHDOWNEY"
    (Just "QLD")
    "RTY"
    (-28.216666666666665)
    152.86666666666667

_RKI_ ::
  VFR_Waypoint
_RKI_ =
  VFR_Waypoint
    "RATTLESNAKE ISLAND"
    (Just "QLD")
    "RKI"
    (-19.033333333333335)
    146.61166666666668

_RCS_ ::
  VFR_Waypoint
_RCS_ =
  VFR_Waypoint
    "RED CLIFFS"
    (Just "VIC")
    "RCS"
    (-34.3)
    142.21666666666667

_RDHI_ ::
  VFR_Waypoint
_RDHI_ =
  VFR_Waypoint
    "RED HILL"
    (Just "QLD")
    "RDHI"
    (-21.633333333333333)
    148.05

_RER_ ::
  VFR_Waypoint
_RER_ =
  VFR_Waypoint
    "RED ROCK"
    (Just "NSW")
    "RER"
    (-29.986666666666668)
    153.225

_REDC_ ::
  VFR_Waypoint
_REDC_ =
  VFR_Waypoint
    "REDCLIFFE BRIDGE"
    (Just "WA")
    "REDC"
    (-31.93)
    115.93833333333333

_RDV_ ::
  VFR_Waypoint
_RDV_ =
  VFR_Waypoint
    "REDCLIFFE VALE HS"
    (Just "QLD")
    "RDV"
    (-21.116666666666667)
    148.11666666666667

_REDF_ ::
  VFR_Waypoint
_REDF_ =
  VFR_Waypoint
    "REDFERN RAILWAY STATION"
    (Just "NSW")
    "REDF"
    (-33.891666666666666)
    151.19833333333332

_REDB_ ::
  VFR_Waypoint
_REDB_ =
  VFR_Waypoint
    "REDLAND BAY"
    (Just "QLD")
    "REDB"
    (-27.6)
    153.3

_REDL_ ::
  VFR_Waypoint
_REDL_ =
  VFR_Waypoint
    "REDLYNCH"
    (Just "QLD")
    "REDL"
    (-16.883333333333333)
    145.7

_RENR_ ::
  VFR_Waypoint
_RENR_ =
  VFR_Waypoint
    "RENNER SPRINGS"
    (Just "NT")
    "RENR"
    (-18.316666666666666)
    133.8

_RESC_ ::
  VFR_Waypoint
_RESC_ =
  VFR_Waypoint
    "RESEARCH CENTRE"
    (Just "NT")
    "RESC"
    (-14.78)
    131.93666666666667

_RCH_ ::
  VFR_Waypoint
_RCH_ =
  VFR_Waypoint
    "RICHMOND"
    (Just "TAS")
    "RCH"
    (-42.733333333333334)
    147.43333333333334

_RIT_ ::
  VFR_Waypoint
_RIT_ =
  VFR_Waypoint
    "RING TANK"
    (Just "QLD")
    "RIT"
    (-26.75)
    153.09166666666667

_RMH_ ::
  VFR_Waypoint
_RMH_ =
  VFR_Waypoint
    "RIVER MOUTH"
    (Just "SA")
    "RMH"
    (-34.583333333333336)
    138.35833333333332

_RIV_ ::
  VFR_Waypoint
_RIV_ =
  VFR_Waypoint
    "RIVERINA CAMPUS"
    (Just "NSW")
    "RIV"
    (-35.055)
    147.34666666666666

_RVTN_ ::
  VFR_Waypoint
_RVTN_ =
  VFR_Waypoint
    "RIVERTON"
    (Just "SA")
    "RVTN"
    (-34.165)
    138.74666666666667

_ROTC_ ::
  VFR_Waypoint
_ROTC_ =
  VFR_Waypoint
    "ROBINA TOWN CENTRE"
    (Just "QLD")
    "ROTC"
    (-28.076666666666668)
    153.385

_ROK_ ::
  VFR_Waypoint
_ROK_ =
  VFR_Waypoint
    "ROCKBANK"
    (Just "VIC")
    "ROK"
    (-37.72833333333333)
    144.65333333333334

_ROHM_ ::
  VFR_Waypoint
_ROHM_ =
  VFR_Waypoint
    "ROCKINGHAM"
    (Just "WA")
    "ROHM"
    (-32.291666666666664)
    115.74166666666666

_RLY_ ::
  VFR_Waypoint
_RLY_ =
  VFR_Waypoint
    "ROLEYSTONE"
    (Just "WA")
    "RLY"
    (-32.11666666666667)
    116.075

_RGS_ ::
  VFR_Waypoint
_RGS_ =
  VFR_Waypoint
    "ROLLINGSTONE"
    (Just "QLD")
    "RGS"
    (-19.046666666666667)
    146.38833333333332

_RKWC_ ::
  VFR_Waypoint
_RKWC_ =
  VFR_Waypoint
    "ROOKWOOD CEMETERY"
    (Just "NSW")
    "RKWC"
    (-33.875)
    151.055

_RSH_ ::
  VFR_Waypoint
_RSH_ =
  VFR_Waypoint
    "ROSEHILL RACECOURSE"
    (Just "NSW")
    "RSH"
    (-33.825)
    151.025

_RVB_ ::
  VFR_Waypoint
_RVB_ =
  VFR_Waypoint
    "ROSEVILLE BRIDGE"
    (Just "NSW")
    "RVB"
    (-33.766666666666666)
    151.2

_RSWD_ ::
  VFR_Waypoint
_RSWD_ =
  VFR_Waypoint
    "ROSEWOOD"
    (Just "QLD")
    "RSWD"
    (-27.636666666666667)
    152.59333333333333

_RSEW_ ::
  VFR_Waypoint
_RSEW_ =
  VFR_Waypoint
    "ROSEWOOD ISLAND"
    (Just "QLD")
    "RSEW"
    (-22.4)
    149.73333333333332

_RRDM_ ::
  VFR_Waypoint
_RRDM_ =
  VFR_Waypoint
    "ROSS RIVER DAM"
    (Just "QLD")
    "RRDM"
    (-19.411666666666665)
    146.73333333333332

_RLR_ ::
  VFR_Waypoint
_RLR_ =
  VFR_Waypoint
    "ROSSLYNNE RESV"
    (Just "VIC")
    "RLR"
    (-37.47)
    144.56333333333333

_RMT_ ::
  VFR_Waypoint
_RMT_ =
  VFR_Waypoint
    "ROUND MT"
    (Just "QLD")
    "RMT"
    (-19.461666666666666)
    146.695

_RCB_ ::
  VFR_Waypoint
_RCB_ =
  VFR_Waypoint
    "RUSH CUTTERS BAY"
    (Just "NSW")
    "RCB"
    (-33.873333333333335)
    151.23166666666665

_RUIS_ ::
  VFR_Waypoint
_RUIS_ =
  VFR_Waypoint
    "RUSSELL ISLAND"
    (Just "QLD")
    "RUIS"
    (-27.666666666666668)
    153.38333333333333

_RYB_ ::
  VFR_Waypoint
_RYB_ =
  VFR_Waypoint
    "RYDE BRIDGE"
    (Just "NSW")
    "RYB"
    (-33.825)
    151.09166666666667

_SADD_ ::
  VFR_Waypoint
_SADD_ =
  VFR_Waypoint
    "SADDLE MT"
    (Just "QLD")
    "SADD"
    (-16.82)
    145.65

_SSV_ ::
  VFR_Waypoint
_SSV_ =
  VFR_Waypoint
    "SAMSONVALE"
    (Just "QLD")
    "SSV"
    (-27.278333333333332)
    152.855

_SAU_ ::
  VFR_Waypoint
_SAU_ =
  VFR_Waypoint
    "SANCTUARY COVE"
    (Just "QLD")
    "SAU"
    (-27.858333333333334)
    153.375

_SALW_ ::
  VFR_Waypoint
_SALW_ =
  VFR_Waypoint
    "SANDALWOOD"
    (Just "SA")
    "SALW"
    (-34.95)
    140.13333333333333

_SSTO_ ::
  VFR_Waypoint
_SSTO_ =
  VFR_Waypoint
    "SANDERSTON"
    (Just "SA")
    "SSTO"
    (-34.74333333333333)
    139.25166666666667

_SAND_ ::
  VFR_Waypoint
_SAND_ =
  VFR_Waypoint
    "SANDGATE PIER"
    (Just "QLD")
    "SAND"
    (-27.328333333333333)
    153.08833333333334

_SDP_ ::
  VFR_Waypoint
_SDP_ =
  VFR_Waypoint
    "SANDY PT"
    (Just "VIC")
    "SDP"
    (-38.416666666666664)
    145.23333333333332

_STT_ ::
  VFR_Waypoint
_STT_ =
  VFR_Waypoint
    "SANTA TERESA"
    (Just "NT")
    "STT"
    (-24.133333333333333)
    134.37333333333333

_SJI_ ::
  VFR_Waypoint
_SJI_ =
  VFR_Waypoint
    "SARAJI"
    (Just "QLD")
    "SJI"
    (-22.433333333333334)
    148.28333333333333

_SRIN_ ::
  VFR_Waypoint
_SRIN_ =
  VFR_Waypoint
    "SARINA"
    (Just "QLD")
    "SRIN"
    (-21.425)
    149.21666666666667

_SVR_ ::
  VFR_Waypoint
_SVR_ =
  VFR_Waypoint
    "SAVAGE RIVER"
    (Just "TAS")
    "SVR"
    (-41.583333333333336)
    145.13333333333333

_SWTE_ ::
  VFR_Waypoint
_SWTE_ =
  VFR_Waypoint
    "SAWTELL"
    (Just "NSW")
    "SWTE"
    (-30.366666666666667)
    153.1

_SWY_ ::
  VFR_Waypoint
_SWY_ =
  VFR_Waypoint
    "SAWYERS VALLEY"
    (Just "WA")
    "SWY"
    (-31.905)
    116.205

_STC_ ::
  VFR_Waypoint
_STC_ =
  VFR_Waypoint
    "SCOTT CREEK"
    (Just "NT")
    "STC"
    (-14.833333333333334)
    131.83333333333334

_STTE_ ::
  VFR_Waypoint
_STTE_ =
  VFR_Waypoint
    "SCOTTSDALE"
    (Just "TAS")
    "STTE"
    (-41.166666666666664)
    147.51666666666668

_SECF_ ::
  VFR_Waypoint
_SECF_ =
  VFR_Waypoint
    "SEA CLIFF BRIDGE"
    (Just "NSW")
    "SECF"
    (-34.25333333333333)
    150.975

_SVAL_ ::
  VFR_Waypoint
_SVAL_ =
  VFR_Waypoint
    "SECOND VALLEY"
    (Just "SA")
    "SVAL"
    (-35.516666666666666)
    138.21666666666667

_SLB_ ::
  VFR_Waypoint
_SLB_ =
  VFR_Waypoint
    "SELLICKS BEACH"
    (Just "SA")
    "SLB"
    (-35.34166666666667)
    138.45

_SDS_ ::
  VFR_Waypoint
_SDS_ =
  VFR_Waypoint
    "SHAUNA DOWNS"
    (Just "QLD")
    "SDS"
    (-24.616666666666667)
    149.91666666666666

_SHCR_ ::
  VFR_Waypoint
_SHCR_ =
  VFR_Waypoint
    "SHAW CREEK"
    (Just "NT")
    "SHCR"
    (-25.216666666666665)
    129.73

_SHI_ ::
  VFR_Waypoint
_SHI_ =
  VFR_Waypoint
    "SHAW ISLAND"
    (Just "QLD")
    "SHI"
    (-20.513333333333332)
    149.08666666666667

_SHL_ ::
  VFR_Waypoint
_SHL_ =
  VFR_Waypoint
    "SHELBURNE BAY"
    (Just "QLD")
    "SHL"
    (-11.883333333333333)
    143.01666666666668

_SEL_ ::
  VFR_Waypoint
_SEL_ =
  VFR_Waypoint
    "SHELLEY BRIDGE"
    (Just "WA")
    "SEL"
    (-32.025)
    115.9

_SHOAL_ ::
  VFR_Waypoint
_SHOAL_ =
  VFR_Waypoint
    "SHOAL"
    (Just "VIC")
    "SHOAL"
    (-38.06666666666667)
    145.03333333333333

_SSG_ ::
  VFR_Waypoint
_SSG_ =
  VFR_Waypoint
    "SIMPSONS GAP"
    (Just "NT")
    "SSG"
    (-23.68)
    133.71666666666667

_SIXS_ ::
  VFR_Waypoint
_SIXS_ =
  VFR_Waypoint
    "SIX SOUTH"
    (Just "WA")
    "SIXS"
    (-32.185)
    115.93333333333334

_SKP_ ::
  VFR_Waypoint
_SKP_ =
  VFR_Waypoint
    "SKIPTON"
    (Just "VIC")
    "SKP"
    (-37.68333333333333)
    143.36666666666667

_SLPT_ ::
  VFR_Waypoint
_SLPT_ =
  VFR_Waypoint
    "SLADE POINT"
    (Just "QLD")
    "SLPT"
    (-21.065)
    149.225

_SGK_ ::
  VFR_Waypoint
_SGK_ =
  VFR_Waypoint
    "SLOPING HUMMOCK"
    (Just "QLD")
    "SGK"
    (-24.85)
    152.43333333333334

_SLP_ ::
  VFR_Waypoint
_SLP_ =
  VFR_Waypoint
    "SLOPING ISLAND"
    (Just "TAS")
    "SLP"
    (-42.94833333333333)
    147.64833333333334

_SMIF_ ::
  VFR_Waypoint
_SMIF_ =
  VFR_Waypoint
    "SMITHFIELD"
    (Just "QLD")
    "SMIF"
    (-16.833333333333332)
    145.68333333333334

_SYI_ ::
  VFR_Waypoint
_SYI_ =
  VFR_Waypoint
    "SNOWY INTERSECTION"
    (Just "NSW")
    "SYI"
    (-35.18333333333333)
    147.86833333333334

_SOFA_ ::
  VFR_Waypoint
_SOFA_ =
  VFR_Waypoint
    "SOFALA"
    (Just "NSW")
    "SOFA"
    (-33.016666666666666)
    149.68333333333334

_SRP_ ::
  VFR_Waypoint
_SRP_ =
  VFR_Waypoint
    "SOLDIERS POINT"
    (Just "NSW")
    "SRP"
    (-32.7)
    152.06333333333333

_SMD_ ::
  VFR_Waypoint
_SMD_ =
  VFR_Waypoint
    "SOMERSET DAM"
    (Just "QLD")
    "SMD"
    (-27.121666666666666)
    152.55

_SMN_ ::
  VFR_Waypoint
_SMN_ =
  VFR_Waypoint
    "SOMERTON"
    (Just "NSW")
    "SMN"
    (-30.941666666666666)
    150.63666666666666

_SORL_ ::
  VFR_Waypoint
_SORL_ =
  VFR_Waypoint
    "SORELL"
    (Just "TAS")
    "SORL"
    (-42.78333333333333)
    147.58333333333334

_SEC_ ::
  VFR_Waypoint
_SEC_ =
  VFR_Waypoint
    "SOUTH EAST CAPE"
    (Just "TAS")
    "SEC"
    (-43.65)
    146.81666666666666

_SMIB_ ::
  VFR_Waypoint
_SMIB_ =
  VFR_Waypoint
    "SOUTH MISSION BEACH"
    (Just "QLD")
    "SMIB"
    (-17.948333333333334)
    146.09166666666667

_SPR_ ::
  VFR_Waypoint
_SPR_ =
  VFR_Waypoint
    "SOUTH PARA RESV"
    (Just "SA")
    "SPR"
    (-34.68333333333333)
    138.86666666666667

_SPN_ ::
  VFR_Waypoint
_SPN_ =
  VFR_Waypoint
    "SOUTH PINNACLE"
    (Just "QLD")
    "SPN"
    (-19.408333333333335)
    146.63333333333333

_SDG_ ::
  VFR_Waypoint
_SDG_ =
  VFR_Waypoint
    "SOUTHEDGE"
    (Just "QLD")
    "SDG"
    (-16.816666666666666)
    145.21666666666667

_SBRR_ ::
  VFR_Waypoint
_SBRR_ =
  VFR_Waypoint
    "SOUTHERN TIP BERSERKERS"
    (Just "QLD")
    "SBRR"
    (-23.4)
    150.625

_SPT_ ::
  VFR_Waypoint
_SPT_ =
  VFR_Waypoint
    "SOUTHPORT"
    (Just "QLD")
    "SPT"
    (-27.916666666666668)
    153.37166666666667

_STR_ ::
  VFR_Waypoint
_STR_ =
  VFR_Waypoint
    "SOUTHPORT ROAD"
    (Just "NT")
    "STR"
    (-12.65)
    130.775

_SPIT_ ::
  VFR_Waypoint
_SPIT_ =
  VFR_Waypoint
    "SPIT BRIDGE"
    (Just "NSW")
    "SPIT"
    (-33.803333333333335)
    151.24666666666667

_SRR_ ::
  VFR_Waypoint
_SRR_ =
  VFR_Waypoint
    "SPLIT ROCK RESV"
    (Just "NSW")
    "SRR"
    (-30.575)
    150.7

_SOI_ ::
  VFR_Waypoint
_SOI_ =
  VFR_Waypoint
    "SPLIT SOLITARY ISLAND"
    (Just "NSW")
    "SOI"
    (-30.241666666666667)
    153.18

_SPMT_ ::
  VFR_Waypoint
_SPMT_ =
  VFR_Waypoint
    "SPRING MOUNTAIN"
    (Just "QLD")
    "SPMT"
    (-27.713333333333335)
    152.885

_SBK_ ::
  VFR_Waypoint
_SBK_ =
  VFR_Waypoint
    "SPRINGBROOK"
    (Just "NSW")
    "SBK"
    (-28.231666666666666)
    153.27833333333334

_SGM_ ::
  VFR_Waypoint
_SGM_ =
  VFR_Waypoint
    "ST GEORGES MINE"
    (Just "QLD")
    "SGM"
    (-16.5)
    144.4

_SHIS_ ::
  VFR_Waypoint
_SHIS_ =
  VFR_Waypoint
    "ST HELENA ISLAND"
    (Just "QLD")
    "SHIS"
    (-27.378333333333334)
    153.23333333333332

_SIS_ ::
  VFR_Waypoint
_SIS_ =
  VFR_Waypoint
    "ST IVES SHOWGROUND"
    (Just "NSW")
    "SIS"
    (-33.705)
    151.18333333333334

_SKI_ ::
  VFR_Waypoint
_SKI_ =
  VFR_Waypoint
    "ST KILDA"
    (Just "SA")
    "SKI"
    (-34.74166666666667)
    138.53

_SRY_ ::
  VFR_Waypoint
_SRY_ =
  VFR_Waypoint
    "STANSBURY"
    (Just "SA")
    "SRY"
    (-34.91166666666667)
    137.78833333333333

_SLL_ ::
  VFR_Waypoint
_SLL_ =
  VFR_Waypoint
    "STANWELL PARK"
    (Just "NSW")
    "SLL"
    (-34.22833333333333)
    150.98833333333334

_SPS_ ::
  VFR_Waypoint
_SPS_ =
  VFR_Waypoint
    "STANWELL POWER STN"
    (Just "QLD")
    "SPS"
    (-23.5)
    150.33333333333334

_STARF_ ::
  VFR_Waypoint
_STARF_ =
  VFR_Waypoint
    "STARF"
    Nothing
    "STARF"
    (-9.126666666666667)
    146.725

_SNP_ ::
  VFR_Waypoint
_SNP_ =
  VFR_Waypoint
    "STATION PIER"
    (Just "VIC")
    "SNP"
    (-37.848333333333336)
    144.93

_STPK_ ::
  VFR_Waypoint
_STPK_ =
  VFR_Waypoint
    "STEPHENSONS PEAK"
    (Just "NT")
    "STPK"
    (-25.5)
    130.18333333333334

_SCRK_ ::
  VFR_Waypoint
_SCRK_ =
  VFR_Waypoint
    "STONEY CREEK"
    (Just "QLD")
    "SCRK"
    (-16.878333333333334)
    145.655

_SBD_ ::
  VFR_Waypoint
_SBD_ =
  VFR_Waypoint
    "STORY BRIDGE"
    (Just "QLD")
    "SBD"
    (-27.465)
    153.03833333333333

_STOT_ ::
  VFR_Waypoint
_STOT_ =
  VFR_Waypoint
    "STOTTS ISLAND"
    (Just "NSW")
    "STOT"
    (-28.268333333333334)
    153.5

_SYN_ ::
  VFR_Waypoint
_SYN_ =
  VFR_Waypoint
    "STRATHALBYN"
    (Just "SA")
    "SYN"
    (-35.25833333333333)
    138.895

_SFE_ ::
  VFR_Waypoint
_SFE_ =
  VFR_Waypoint
    "STRATHFINELLA"
    (Just "QLD")
    "SFE"
    (-23.35)
    143.55

_STRA_ ::
  VFR_Waypoint
_STRA_ =
  VFR_Waypoint
    "STRATHGORDON"
    (Just "TAS")
    "STRA"
    (-42.766666666666666)
    146.03666666666666

_SLY_ ::
  VFR_Waypoint
_SLY_ =
  VFR_Waypoint
    "STRELLEY HS"
    (Just "WA")
    "SLY"
    (-20.441666666666666)
    118.98333333333333

_SRO_ ::
  VFR_Waypoint
_SRO_ =
  VFR_Waypoint
    "STROUD ROAD"
    (Just "NSW")
    "SRO"
    (-32.346666666666664)
    151.91833333333332

_SUA_ ::
  VFR_Waypoint
_SUA_ =
  VFR_Waypoint
    "STUART"
    (Just "QLD")
    "SUA"
    (-19.35)
    146.83333333333334

_STUM_ ::
  VFR_Waypoint
_STUM_ =
  VFR_Waypoint
    "STUMERS CREEK"
    (Just "QLD")
    "STUM"
    (-26.52)
    153.085

_SUI_ ::
  VFR_Waypoint
_SUI_ =
  VFR_Waypoint
    "STURT INTERSECTION"
    (Just "NSW")
    "SUI"
    (-35.22)
    147.79333333333332

_SUB_ ::
  VFR_Waypoint
_SUB_ =
  VFR_Waypoint
    "SUBSTATION"
    (Just "SA")
    "SUB"
    (-34.736666666666665)
    138.71333333333334

_SUTR_ ::
  VFR_Waypoint
_SUTR_ =
  VFR_Waypoint
    "SUGAR TERMINAL"
    (Just "QLD")
    "SUTR"
    (-16.941666666666666)
    145.76666666666668

_SLMT_ ::
  VFR_Waypoint
_SLMT_ =
  VFR_Waypoint
    "SUGARLOAF MT"
    (Just "NSW")
    "SLMT"
    (-31.433333333333334)
    150.875

_SUG_ ::
  VFR_Waypoint
_SUG_ =
  VFR_Waypoint
    "SUGARLOAF PT"
    (Just "NSW")
    "SUG"
    (-32.445)
    152.54

_SGSV_ ::
  VFR_Waypoint
_SGSV_ =
  VFR_Waypoint
    "SUGARLOAF RESERVOIR"
    (Just "VIC")
    "SGSV"
    (-37.675)
    145.3

_SWLD_ ::
  VFR_Waypoint
_SWLD_ =
  VFR_Waypoint
    "SUGARWORLD"
    (Just "QLD")
    "SWLD"
    (-17.016666666666666)
    145.73166666666665

_SUNZ_ ::
  VFR_Waypoint
_SUNZ_ =
  VFR_Waypoint
    "SUN ZINC REFINERY"
    (Just "QLD")
    "SUNZ"
    (-19.333333333333332)
    146.88666666666666

_SBU_ ::
  VFR_Waypoint
_SBU_ =
  VFR_Waypoint
    "SUNBURY"
    (Just "VIC")
    "SBU"
    (-37.583333333333336)
    144.725

_SWT_ ::
  VFR_Waypoint
_SWT_ =
  VFR_Waypoint
    "SUNBURY WATER TANK"
    (Just "VIC")
    "SWT"
    (-37.54833333333333)
    144.69166666666666

_SBIT_ ::
  VFR_Waypoint
_SBIT_ =
  VFR_Waypoint
    "SURBITON"
    (Just "QLD")
    "SBIT"
    (-23.15)
    146.61666666666667

_SUPA_ ::
  VFR_Waypoint
_SUPA_ =
  VFR_Waypoint
    "SURFER'S PARADISE"
    (Just "QLD")
    "SUPA"
    (-28.0)
    153.43333333333334

_SUD_ ::
  VFR_Waypoint
_SUD_ =
  VFR_Waypoint
    "SUTHERLAND"
    (Just "NSW")
    "SUD"
    (-34.038333333333334)
    151.055

_SHER_ ::
  VFR_Waypoint
_SHER_ =
  VFR_Waypoint
    "SUTHERLANDS"
    (Just "SA")
    "SHER"
    (-34.15)
    139.23333333333332

_SUE_ ::
  VFR_Waypoint
_SUE_ =
  VFR_Waypoint
    "SUTTON ROAD OVERPASS"
    (Just "NSW")
    "SUE"
    (-35.18333333333333)
    149.25833333333333

_SBCH_ ::
  VFR_Waypoint
_SBCH_ =
  VFR_Waypoint
    "SUTTONS BEACH"
    (Just "QLD")
    "SBCH"
    (-27.235)
    153.115

_SCTY_ ::
  VFR_Waypoint
_SCTY_ =
  VFR_Waypoint
    "SYDNEY CBD"
    (Just "NSW")
    "SCTY"
    (-33.86666666666667)
    151.2

_SCG_ ::
  VFR_Waypoint
_SCG_ =
  VFR_Waypoint
    "SYDNEY CRICKET GROUND"
    (Just "NSW")
    "SCG"
    (-33.891666666666666)
    151.22333333333333

_SYHD_ ::
  VFR_Waypoint
_SYHD_ =
  VFR_Waypoint
    "SYDNEY HEADS"
    (Just "NSW")
    "SYHD"
    (-33.833333333333336)
    151.29166666666666

_SYP_ ::
  VFR_Waypoint
_SYP_ =
  VFR_Waypoint
    "SYMMONS PLAINS"
    (Just "TAS")
    "SYP"
    (-41.655)
    147.25

_TBL_ ::
  VFR_Waypoint
_TBL_ =
  VFR_Waypoint
    "TABULAM"
    (Just "NSW")
    "TBL"
    (-28.886666666666667)
    152.56833333333333

_TCH_ ::
  VFR_Waypoint
_TCH_ =
  VFR_Waypoint
    "TALC HEAD"
    (Just "NT")
    "TCH"
    (-12.48)
    130.76666666666668

_TLAG_ ::
  VFR_Waypoint
_TLAG_ =
  VFR_Waypoint
    "TALLANGATTA"
    (Just "VIC")
    "TLAG"
    (-36.21666666666667)
    147.175

_TLY_ ::
  VFR_Waypoint
_TLY_ =
  VFR_Waypoint
    "TALLANGATTA CAUSEWAY"
    (Just "VIC")
    "TLY"
    (-36.21333333333333)
    147.24666666666667

_TRWL_ ::
  VFR_Waypoint
_TRWL_ =
  VFR_Waypoint
    "TALLARINGA WELL"
    (Just "SA")
    "TRWL"
    (-29.033333333333335)
    133.28333333333333

_TLK_ ::
  VFR_Waypoint
_TLK_ =
  VFR_Waypoint
    "TALLAROOK"
    (Just "VIC")
    "TLK"
    (-37.1)
    145.1

_TID_ ::
  VFR_Waypoint
_TID_ =
  VFR_Waypoint
    "TAMAR ISLAND"
    (Just "TAS")
    "TID"
    (-41.38333333333333)
    147.08333333333334

_TGN_ ::
  VFR_Waypoint
_TGN_ =
  VFR_Waypoint
    "TANGORIN"
    (Just "QLD")
    "TGN"
    (-21.733333333333334)
    144.2

_TUND_ ::
  VFR_Waypoint
_TUND_ =
  VFR_Waypoint
    "TANUNDA"
    (Just "SA")
    "TUND"
    (-34.53333333333333)
    138.96666666666667

_TAGO_ ::
  VFR_Waypoint
_TAGO_ =
  VFR_Waypoint
    "TARAGO"
    (Just "NSW")
    "TAGO"
    (-35.07)
    149.655

_TOWI_ ::
  VFR_Waypoint
_TOWI_ =
  VFR_Waypoint
    "TARCOWIE"
    (Just "SA")
    "TOWI"
    (-32.95)
    138.51666666666668

_TRT_ ::
  VFR_Waypoint
_TRT_ =
  VFR_Waypoint
    "TARCUTTA"
    (Just "NSW")
    "TRT"
    (-35.28)
    147.73833333333334

_TAP_ ::
  VFR_Waypoint
_TAP_ =
  VFR_Waypoint
    "TARGA GAP"
    (Just "TAS")
    "TAP"
    (-41.31166666666667)
    147.36833333333334

_TAR_ ::
  VFR_Waypoint
_TAR_ =
  VFR_Waypoint
    "TARGET"
    (Just "QLD")
    "TAR"
    (-27.613333333333333)
    153.12666666666667

_TLEE_ ::
  VFR_Waypoint
_TLEE_ =
  VFR_Waypoint
    "TARLEE"
    (Just "SA")
    "TLEE"
    (-34.275)
    138.76833333333335

_TAS_ ::
  VFR_Waypoint
_TAS_ =
  VFR_Waypoint
    "TASMAN BRIDGE"
    (Just "TAS")
    "TAS"
    (-42.86666666666667)
    147.35

_CKO_ ::
  VFR_Waypoint
_CKO_ =
  VFR_Waypoint
    "TELEGRAPH OFFICE"
    (Just "QLD")
    "CKO"
    (-19.233333333333334)
    145.48333333333332

_TPL_ ::
  VFR_Waypoint
_TPL_ =
  VFR_Waypoint
    "TEMPLE BAY"
    (Just "QLD")
    "TPL"
    (-12.266666666666667)
    143.15

_TLC_ ::
  VFR_Waypoint
_TLC_ =
  VFR_Waypoint
    "TERRANORA LAKES COUNTRY CLUB"
    (Just "QLD")
    "TLC"
    (-28.215)
    153.47333333333333

_TWT_ ::
  VFR_Waypoint
_TWT_ =
  VFR_Waypoint
    "TEWANTIN"
    (Just "QLD")
    "TWT"
    (-26.4)
    153.03333333333333

_THW_ ::
  VFR_Waypoint
_THW_ =
  VFR_Waypoint
    "THARWA"
    (Just "ACT")
    "THW"
    (-35.513333333333335)
    149.07

_TCNR_ ::
  VFR_Waypoint
_TCNR_ =
  VFR_Waypoint
    "THE CORNER"
    (Just "QLD")
    "TCNR"
    (-19.325)
    146.72833333333332

_THUM_ ::
  VFR_Waypoint
_THUM_ =
  VFR_Waypoint
    "THE GUMS"
    (Just "SA")
    "THUM"
    (-33.833333333333336)
    139.33333333333334

_TGU_ ::
  VFR_Waypoint
_TGU_ =
  VFR_Waypoint
    "THE GUMS HS"
    (Just "SA")
    "TGU"
    (-33.85)
    139.35

_TLMI_ ::
  VFR_Waypoint
_TLMI_ =
  VFR_Waypoint
    "THE LAKES MINE"
    (Just "WA")
    "TLMI"
    (-31.865)
    116.35833333333333

_THK_ ::
  VFR_Waypoint
_THK_ =
  VFR_Waypoint
    "THE OAKS"
    (Just "NSW")
    "THK"
    (-34.07833333333333)
    150.57833333333335

_PIN_ ::
  VFR_Waypoint
_PIN_ =
  VFR_Waypoint
    "THE PINES"
    (Just "QLD")
    "PIN"
    (-28.141666666666666)
    153.46666666666667

_RCK_ ::
  VFR_Waypoint
_RCK_ =
  VFR_Waypoint
    "THE ROCK"
    (Just "NSW")
    "RCK"
    (-35.275)
    147.07166666666666

_APST_ ::
  VFR_Waypoint
_APST_ =
  VFR_Waypoint
    "THE STAMFORD (HOTEL)"
    (Just "NSW")
    "APST"
    (-33.931666666666665)
    151.185

_WHF_ ::
  VFR_Waypoint
_WHF_ =
  VFR_Waypoint
    "THE WHARF"
    (Just "VIC")
    "WHF"
    (-38.1)
    144.53333333333333

_THB_ ::
  VFR_Waypoint
_THB_ =
  VFR_Waypoint
    "THEEBINE"
    (Just "QLD")
    "THB"
    (-25.95)
    152.55

_THSM_ ::
  VFR_Waypoint
_THSM_ =
  VFR_Waypoint
    "THOMPSON 1"
    (Just "QLD")
    "THSM"
    (-17.011666666666667)
    145.75166666666667

_TORN_ ::
  VFR_Waypoint
_TORN_ =
  VFR_Waypoint
    "THORNTON"
    (Just "QLD")
    "TORN"
    (-27.816666666666666)
    152.38333333333333

_TNP_ ::
  VFR_Waypoint
_TNP_ =
  VFR_Waypoint
    "THORNTON GAP"
    (Just "QLD")
    "TNP"
    (-19.358333333333334)
    146.46166666666667

_TBRT_ ::
  VFR_Waypoint
_TBRT_ =
  VFR_Waypoint
    "TIMBERTOP"
    (Just "QLD")
    "TBRT"
    (-16.958333333333332)
    145.805

_TING_ ::
  VFR_Waypoint
_TING_ =
  VFR_Waypoint
    "TINGALPA RESERVOIR"
    (Just "QLD")
    "TING"
    (-27.54)
    153.16666666666666

_TMPT_ ::
  VFR_Waypoint
_TMPT_ =
  VFR_Waypoint
    "TOM PRICE"
    (Just "WA")
    "TMPT"
    (-22.695)
    117.79166666666667

_TOO_ ::
  VFR_Waypoint
_TOO_ =
  VFR_Waypoint
    "TOOBORAC"
    (Just "VIC")
    "TOO"
    (-37.05)
    144.8

_TLN_ ::
  VFR_Waypoint
_TLN_ =
  VFR_Waypoint
    "TOOLLEEN"
    (Just "VIC")
    "TLN"
    (-36.71666666666667)
    144.68333333333334

_TOOU_ ::
  VFR_Waypoint
_TOOU_ =
  VFR_Waypoint
    "TOOMULLA"
    (Just "QLD")
    "TOOU"
    (-19.083333333333332)
    146.46666666666667

_TOGA_ ::
  VFR_Waypoint
_TOGA_ =
  VFR_Waypoint
    "TOORONGA"
    (Just "VIC")
    "TOGA"
    (-37.848333333333336)
    145.04666666666665

_TOWA_ ::
  VFR_Waypoint
_TOWA_ =
  VFR_Waypoint
    "TOWRANA HS"
    (Just "WA")
    "TOWA"
    (-25.433333333333334)
    115.23333333333333

_TVM_ ::
  VFR_Waypoint
_TVM_ =
  VFR_Waypoint
    "TREVALLYN DAM"
    (Just "TAS")
    "TVM"
    (-41.455)
    147.08833333333334

_TRIN_ ::
  VFR_Waypoint
_TRIN_ =
  VFR_Waypoint
    "TRINITY BEACH"
    (Just "QLD")
    "TRIN"
    (-16.783333333333335)
    145.7

_TRUO_ ::
  VFR_Waypoint
_TRUO_ =
  VFR_Waypoint
    "TRURO"
    (Just "SA")
    "TRUO"
    (-34.41166666666667)
    139.12

_TKER_ ::
  VFR_Waypoint
_TKER_ =
  VFR_Waypoint
    "TUCKER"
    (Just "QLD")
    "TKER"
    (-24.483333333333334)
    149.2

_TUK_ ::
  VFR_Waypoint
_TUK_ =
  VFR_Waypoint
    "TUCKERS KNOB"
    (Just "NSW")
    "TUK"
    (-30.343333333333334)
    152.98

_TGC_ ::
  VFR_Waypoint
_TGC_ =
  VFR_Waypoint
    "TUGGERANONG TOWN CENTRE"
    (Just "ACT")
    "TGC"
    (-35.416666666666664)
    149.06666666666666

_TUMB_ ::
  VFR_Waypoint
_TUMB_ =
  VFR_Waypoint
    "TUMBULGUM"
    (Just "NSW")
    "TUMB"
    (-28.278333333333332)
    153.46666666666667

_TTLE_ ::
  VFR_Waypoint
_TTLE_ =
  VFR_Waypoint
    "TURTLE PT"
    (Just "NT")
    "TTLE"
    (-14.85)
    129.25

_TWRN_ ::
  VFR_Waypoint
_TWRN_ =
  VFR_Waypoint
    "TWO RN"
    (Just "NSW")
    "TWRN"
    (-33.93666666666667)
    150.88833333333332

_TOS_ ::
  VFR_Waypoint
_TOS_ =
  VFR_Waypoint
    "TWO ROCKS"
    (Just "WA")
    "TOS"
    (-31.491666666666667)
    115.58333333333333

_UKER_ ::
  VFR_Waypoint
_UKER_ =
  VFR_Waypoint
    "UKEREBAGH ISLAND"
    (Just "NSW")
    "UKER"
    (-28.18)
    153.54666666666665

_UDA_ ::
  VFR_Waypoint
_UDA_ =
  VFR_Waypoint
    "ULLADULLA"
    (Just "NSW")
    "UDA"
    (-35.35)
    150.48333333333332

_UNDW_ ::
  VFR_Waypoint
_UNDW_ =
  VFR_Waypoint
    "UNDEMOW WATERHOLE"
    (Just "NT")
    "UNDW"
    (-17.583333333333332)
    135.25

_UQLD_ ::
  VFR_Waypoint
_UQLD_ =
  VFR_Waypoint
    "UNI OF QLD ST LUCIA"
    (Just "QLD")
    "UQLD"
    (-27.498333333333335)
    153.01333333333332

_USW_ ::
  VFR_Waypoint
_USW_ =
  VFR_Waypoint
    "UPPER SWAN"
    (Just "WA")
    "USW"
    (-31.77166666666667)
    116.01666666666667

_URAN_ ::
  VFR_Waypoint
_URAN_ =
  VFR_Waypoint
    "URANGAN"
    (Just "QLD")
    "URAN"
    (-25.283333333333335)
    152.9

_URA_ ::
  VFR_Waypoint
_URA_ =
  VFR_Waypoint
    "URANQUINTY"
    (Just "NSW")
    "URA"
    (-35.19166666666667)
    147.24666666666667

_URC_ ::
  VFR_Waypoint
_URC_ =
  VFR_Waypoint
    "URRBRAE AGRICULTURAL HIGH SCHOOL"
    (Just "SA")
    "URC"
    (-34.96666666666667)
    138.625

_URU_ ::
  VFR_Waypoint
_URU_ =
  VFR_Waypoint
    "URUNGA"
    (Just "NSW")
    "URU"
    (-30.491666666666667)
    153.01666666666668

_VTG_ ::
  VFR_Waypoint
_VTG_ =
  VFR_Waypoint
    "VEHICLE TESTING GROUND"
    (Just "VIC")
    "VTG"
    (-37.88333333333333)
    144.41666666666666

_VELO_ ::
  VFR_Waypoint
_VELO_ =
  VFR_Waypoint
    "VELODROME"
    (Just "SA")
    "VELO"
    (-34.843333333333334)
    138.61166666666668

_VICP_ ::
  VFR_Waypoint
_VICP_ =
  VFR_Waypoint
    "VICTORIA PARK"
    (Just "SA")
    "VICP"
    (-34.93333333333333)
    138.62166666666667

_VOKH_ ::
  VFR_Waypoint
_VOKH_ =
  VFR_Waypoint
    "VOKES HILL"
    (Just "SA")
    "VOKH"
    (-28.483333333333334)
    130.58333333333334

_VPH_ ::
  VFR_Waypoint
_VPH_ =
  VFR_Waypoint
    "VPH"
    (Just "WA")
    "VPH"
    (-31.945)
    115.96

_WGL_ ::
  VFR_Waypoint
_WGL_ =
  VFR_Waypoint
    "WAIGEN LAKES"
    (Just "WA")
    "WGL"
    (-27.616666666666667)
    128.78333333333333

_WTC_ ::
  VFR_Waypoint
_WTC_ =
  VFR_Waypoint
    "WAITE CAMPUS"
    (Just "SA")
    "WTC"
    (-34.97)
    138.635

_WKT_ ::
  VFR_Waypoint
_WKT_ =
  VFR_Waypoint
    "WALKERSTON"
    (Just "QLD")
    "WKT"
    (-21.163333333333334)
    149.06333333333333

_WBH_ ::
  VFR_Waypoint
_WBH_ =
  VFR_Waypoint
    "WALLABADAH"
    (Just "NSW")
    "WBH"
    (-31.538333333333334)
    150.825

_WAN_ ::
  VFR_Waypoint
_WAN_ =
  VFR_Waypoint
    "WALLAN"
    (Just "VIC")
    "WAN"
    (-37.40833333333333)
    144.97666666666666

_WGR_ ::
  VFR_Waypoint
_WGR_ =
  VFR_Waypoint
    "WALLANGARRA"
    (Just "NSW")
    "WGR"
    (-28.916666666666668)
    151.93333333333334

_WRO_ ::
  VFR_Waypoint
_WRO_ =
  VFR_Waypoint
    "WALLAROO"
    (Just "SA")
    "WRO"
    (-33.93333333333333)
    137.63333333333333

_WMB_ ::
  VFR_Waypoint
_WMB_ =
  VFR_Waypoint
    "WALLUMBILLA"
    (Just "QLD")
    "WMB"
    (-26.583333333333332)
    149.18333333333334

_WPE_ ::
  VFR_Waypoint
_WPE_ =
  VFR_Waypoint
    "WALPOLE"
    (Just "WA")
    "WPE"
    (-34.983333333333334)
    116.73333333333333

_WTBG_ ::
  VFR_Waypoint
_WTBG_ =
  VFR_Waypoint
    "WALTER TAYLOR BRIDGE"
    (Just "QLD")
    "WTBG"
    (-27.505)
    152.97333333333333

_WDN_ ::
  VFR_Waypoint
_WDN_ =
  VFR_Waypoint
    "WANDANDIAN"
    (Just "NSW")
    "WDN"
    (-35.083333333333336)
    150.51666666666668

_WAND_ ::
  VFR_Waypoint
_WAND_ =
  VFR_Waypoint
    "WANDERING"
    (Just "WA")
    "WAND"
    (-32.675)
    116.66666666666667

_WAT_ ::
  VFR_Waypoint
_WAT_ =
  VFR_Waypoint
    "WANGETTI"
    (Just "QLD")
    "WAT"
    (-16.665)
    145.56666666666666

_WAG_ ::
  VFR_Waypoint
_WAG_ =
  VFR_Waypoint
    "WANTABADGERY"
    (Just "NSW")
    "WAG"
    (-35.05833333333333)
    147.72166666666666

_WRNA_ ::
  VFR_Waypoint
_WRNA_ =
  VFR_Waypoint
    "WAROONA"
    (Just "WA")
    "WRNA"
    (-32.84166666666667)
    115.91666666666667

_WAD_ ::
  VFR_Waypoint
_WAD_ =
  VFR_Waypoint
    "WARRAGAMBA DAM"
    (Just "NSW")
    "WAD"
    (-33.885)
    150.59166666666667

_WASL_ ::
  VFR_Waypoint
_WASL_ =
  VFR_Waypoint
    "WARRAL SILO"
    (Just "NSW")
    "WASL"
    (-31.15)
    150.85833333333332

_WRD_ ::
  VFR_Waypoint
_WRD_ =
  VFR_Waypoint
    "WARRANDYTE"
    (Just "VIC")
    "WRD"
    (-37.75)
    145.20833333333334

_WRR_ ::
  VFR_Waypoint
_WRR_ =
  VFR_Waypoint
    "WARREN RESERVOIR"
    (Just "SA")
    "WRR"
    (-34.708333333333336)
    138.93333333333334

_WFM_ ::
  VFR_Waypoint
_WFM_ =
  VFR_Waypoint
    "WARWICK FARM"
    (Just "NSW")
    "WFM"
    (-33.91166666666667)
    150.94666666666666

_WFL_ ::
  VFR_Waypoint
_WFL_ =
  VFR_Waypoint
    "WATERFALL"
    (Just "NSW")
    "WFL"
    (-34.13666666666666)
    150.99166666666667

_WAYS_ ::
  VFR_Waypoint
_WAYS_ =
  VFR_Waypoint
    "WAYSIDE"
    (Just "NT")
    "WAYS"
    (-15.575)
    131.04

_WWN_ ::
  VFR_Waypoint
_WWN_ =
  VFR_Waypoint
    "WEALWANDANGIE"
    (Just "QLD")
    "WWN"
    (-24.416666666666668)
    148.05

_WCP_ ::
  VFR_Waypoint
_WCP_ =
  VFR_Waypoint
    "WELLCAMP DOWNS"
    (Just "QLD")
    "WCP"
    (-27.55)
    151.85

_WELL_ ::
  VFR_Waypoint
_WELL_ =
  VFR_Waypoint
    "WELLINGTON PT"
    (Just "QLD")
    "WELL"
    (-27.468333333333334)
    153.24166666666667

_WLS_ ::
  VFR_Waypoint
_WLS_ =
  VFR_Waypoint
    "WELLSHOT"
    (Just "QLD")
    "WLS"
    (-23.9)
    144.43333333333334

_WELS_ ::
  VFR_Waypoint
_WELS_ =
  VFR_Waypoint
    "WELSHPOOL"
    (Just "VIC")
    "WELS"
    (-38.665)
    146.43833333333333

_WBER_ ::
  VFR_Waypoint
_WBER_ =
  VFR_Waypoint
    "WERRIBEE RACECOURSE"
    (Just "VIC")
    "WBER"
    (-37.9)
    144.64166666666668

_WBES_ ::
  VFR_Waypoint
_WBES_ =
  VFR_Waypoint
    "WERRIBEE SOUTH"
    (Just "VIC")
    "WBES"
    (-37.97666666666667)
    144.68833333333333

_WEK_ ::
  VFR_Waypoint
_WEK_ =
  VFR_Waypoint
    "WERRIS CREEK"
    (Just "NSW")
    "WEK"
    (-31.358333333333334)
    150.65

_WSM_ ::
  VFR_Waypoint
_WSM_ =
  VFR_Waypoint
    "WEST ARM"
    (Just "NT")
    "WSM"
    (-12.55)
    130.78833333333333

_WEBS_ ::
  VFR_Waypoint
_WEBS_ =
  VFR_Waypoint
    "WEST BASS"
    (Just "VIC")
    "WEBS"
    (-39.5)
    141.0

_WTG_ ::
  VFR_Waypoint
_WTG_ =
  VFR_Waypoint
    "WEST GAP"
    (Just "TAS")
    "WTG"
    (-41.345)
    146.775

_WSN_ ::
  VFR_Waypoint
_WSN_ =
  VFR_Waypoint
    "WEST LAGOON"
    (Just "TAS")
    "WSN"
    (-41.60333333333333)
    147.02833333333334

_WEP_ ::
  VFR_Waypoint
_WEP_ =
  VFR_Waypoint
    "WEST PT"
    (Just "QLD")
    "WEP"
    (-19.128333333333334)
    146.77833333333334

_WNFE_ ::
  VFR_Waypoint
_WNFE_ =
  VFR_Waypoint
    "WESTERN FREEWAY"
    (Just "QLD")
    "WNFE"
    (-27.575)
    152.945

_WES_ ::
  VFR_Waypoint
_WES_ =
  VFR_Waypoint
    "WESTGATE BRIDGE"
    (Just "VIC")
    "WES"
    (-37.83)
    144.89666666666668

_WMR_ ::
  VFR_Waypoint
_WMR_ =
  VFR_Waypoint
    "WESTMAR"
    (Just "QLD")
    "WMR"
    (-27.916666666666668)
    149.71666666666667

_WST_ ::
  VFR_Waypoint
_WST_ =
  VFR_Waypoint
    "WESTMEAD"
    (Just "NSW")
    "WST"
    (-33.803333333333335)
    150.98666666666668

_WTB_ ::
  VFR_Waypoint
_WTB_ =
  VFR_Waypoint
    "WETHERBY"
    (Just "QLD")
    "WTB"
    (-21.5)
    142.83333333333334

_WHM_ ::
  VFR_Waypoint
_WHM_ =
  VFR_Waypoint
    "WHIM CREEK"
    (Just "WA")
    "WHM"
    (-20.833333333333332)
    117.83333333333333

_WTRK_ ::
  VFR_Waypoint
_WTRK_ =
  VFR_Waypoint
    "WHITE ROCK"
    (Just "QLD")
    "WTRK"
    (-18.778333333333332)
    146.71833333333333

_WHW_ ::
  VFR_Waypoint
_WHW_ =
  VFR_Waypoint
    "WHITEWOOD"
    (Just "QLD")
    "WHW"
    (-21.483333333333334)
    143.6

_WTS_ ::
  VFR_Waypoint
_WTS_ =
  VFR_Waypoint
    "WHITTLESEA"
    (Just "VIC")
    "WTS"
    (-37.516666666666666)
    145.11666666666667

_WIKP_ ::
  VFR_Waypoint
_WIKP_ =
  VFR_Waypoint
    "WICKHAM PT"
    (Just "NT")
    "WIKP"
    (-12.505)
    130.86

_WHPL_ ::
  VFR_Waypoint
_WHPL_ =
  VFR_Waypoint
    "WILD HORSE PLAINS"
    (Just "SA")
    "WHPL"
    (-34.36)
    138.28833333333333

_WILE_ ::
  VFR_Waypoint
_WILE_ =
  VFR_Waypoint
    "WILLEROO"
    (Just "NT")
    "WILE"
    (-15.283333333333333)
    131.565

_WMS_ ::
  VFR_Waypoint
_WMS_ =
  VFR_Waypoint
    "WILLIAMSTOWN"
    (Just "VIC")
    "WMS"
    (-37.87)
    144.91166666666666

_WICK_ ::
  VFR_Waypoint
_WICK_ =
  VFR_Waypoint
    "WILLIE CREEK"
    (Just "WA")
    "WICK"
    (-17.75)
    122.20833333333333

_WIE_ ::
  VFR_Waypoint
_WIE_ =
  VFR_Waypoint
    "WIMMERA"
    (Just "NSW")
    "WIE"
    (-31.158333333333335)
    150.81666666666666

_WDU_ ::
  VFR_Waypoint
_WDU_ =
  VFR_Waypoint
    "WIRRADGURIE"
    (Just "NSW")
    "WDU"
    (-31.9)
    152.06666666666666

_WSFR_ ::
  VFR_Waypoint
_WSFR_ =
  VFR_Waypoint
    "WISEMANS FERRY"
    (Just "NSW")
    "WSFR"
    (-33.38)
    150.98833333333334

_WTHB_ ::
  VFR_Waypoint
_WTHB_ =
  VFR_Waypoint
    "WITHNELL BAY"
    (Just "WA")
    "WTHB"
    (-20.58)
    116.78666666666666

_WHDW_ ::
  VFR_Waypoint
_WHDW_ =
  VFR_Waypoint
    "WIVENHOE DAM WALL"
    (Just "QLD")
    "WHDW"
    (-27.395)
    152.60833333333332

_WBK_ ::
  VFR_Waypoint
_WBK_ =
  VFR_Waypoint
    "WOODBROOK"
    (Just "WA")
    "WBK"
    (-20.911666666666665)
    117.11666666666666

_WOT_ ::
  VFR_Waypoint
_WOT_ =
  VFR_Waypoint
    "WOODGATE"
    (Just "QLD")
    "WOT"
    (-25.116666666666667)
    152.56666666666666

_WOOS_ ::
  VFR_Waypoint
_WOOS_ =
  VFR_Waypoint
    "WOODLANDS GOLF COURSE"
    (Just "VIC")
    "WOOS"
    (-37.99666666666667)
    145.1

_WMP_ ::
  VFR_Waypoint
_WMP_ =
  VFR_Waypoint
    "WOODMAN PT"
    (Just "WA")
    "WMP"
    (-32.141666666666666)
    115.73333333333333

_WYPT_ ::
  VFR_Waypoint
_WYPT_ =
  VFR_Waypoint
    "WOODY PT"
    (Just "QLD")
    "WYPT"
    (-27.265)
    153.10333333333332

_WGG_ ::
  VFR_Waypoint
_WGG_ =
  VFR_Waypoint
    "WOOLGOOLGA"
    (Just "NSW")
    "WGG"
    (-30.113333333333333)
    153.2

_WOI_ ::
  VFR_Waypoint
_WOI_ =
  VFR_Waypoint
    "WOOLLAMIA"
    (Just "NSW")
    "WOI"
    (-35.02)
    150.66

_WNK_ ::
  VFR_Waypoint
_WNK_ =
  VFR_Waypoint
    "WOOMANOOKA"
    (Just "QLD")
    "WNK"
    (-13.733333333333333)
    141.58333333333334

_WAM_ ::
  VFR_Waypoint
_WAM_ =
  VFR_Waypoint
    "WOOMARGAMA"
    (Just "NSW")
    "WAM"
    (-35.833333333333336)
    147.24833333333333

_WRM_ ::
  VFR_Waypoint
_WRM_ =
  VFR_Waypoint
    "WOORIM"
    (Just "QLD")
    "WRM"
    (-27.078333333333333)
    153.20333333333335

_WUN_ ::
  VFR_Waypoint
_WUN_ =
  VFR_Waypoint
    "WUNDOWIE"
    (Just "WA")
    "WUN"
    (-31.766666666666666)
    116.37666666666667

_WUTUL_ ::
  VFR_Waypoint
_WUTUL_ =
  VFR_Waypoint
    "WUTUL"
    (Just "QLD")
    "WUTUL"
    (-27.03)
    151.80833333333334

_WMF_ ::
  VFR_Waypoint
_WMF_ =
  VFR_Waypoint
    "WYMAH FERRY"
    (Just "NSW")
    "WMF"
    (-36.041666666666664)
    147.26333333333332

_WYR_ ::
  VFR_Waypoint
_WYR_ =
  VFR_Waypoint
    "WYREEMA"
    (Just "QLD")
    "WYR"
    (-27.65)
    151.85833333333332

_YMA_ ::
  VFR_Waypoint
_YMA_ =
  VFR_Waypoint
    "YAAMBA"
    (Just "QLD")
    "YMA"
    (-23.133333333333333)
    150.36666666666667

_YBU_ ::
  VFR_Waypoint
_YBU_ =
  VFR_Waypoint
    "YABULU"
    (Just "QLD")
    "YBU"
    (-19.213333333333335)
    146.59666666666666

_YKH_ ::
  VFR_Waypoint
_YKH_ =
  VFR_Waypoint
    "YACKANDANDAH"
    (Just "VIC")
    "YKH"
    (-36.31166666666667)
    146.84166666666667

_ALBO_ ::
  VFR_Waypoint
_ALBO_ =
  VFR_Waypoint
    "YALBOROO"
    (Just "QLD")
    "ALBO"
    (-20.833333333333332)
    148.65

_YYN_ ::
  VFR_Waypoint
_YYN_ =
  VFR_Waypoint
    "YAN YEAN RESV"
    (Just "VIC")
    "YYN"
    (-37.55833333333333)
    145.13833333333332

_ANDA_ ::
  VFR_Waypoint
_ANDA_ =
  VFR_Waypoint
    "YANDARAN"
    (Just "QLD")
    "ANDA"
    (-24.716666666666665)
    152.11666666666667

_YNA_ ::
  VFR_Waypoint
_YNA_ =
  VFR_Waypoint
    "YANDINA"
    (Just "QLD")
    "YNA"
    (-26.563333333333333)
    152.955

_YGB_ ::
  VFR_Waypoint
_YGB_ =
  VFR_Waypoint
    "YANGEBUP LAKE"
    (Just "WA")
    "YGB"
    (-32.12)
    115.83333333333333

_YYM_ ::
  VFR_Waypoint
_YYM_ =
  VFR_Waypoint
    "YARINGA YACHT MARINA"
    (Just "VIC")
    "YYM"
    (-38.24666666666667)
    145.25166666666667

_YBH_ ::
  VFR_Waypoint
_YBH_ =
  VFR_Waypoint
    "YARRABAH"
    (Just "QLD")
    "YBH"
    (-16.916666666666668)
    145.88333333333333

_ASST_ ::
  VFR_Waypoint
_ASST_ =
  VFR_Waypoint
    "YASS TOWNSHIP"
    (Just "NSW")
    "ASST"
    (-34.85)
    148.91666666666666

_ORKT_ ::
  VFR_Waypoint
_ORKT_ =
  VFR_Waypoint
    "YORK TOWNSHIP"
    (Just "WA")
    "ORKT"
    (-31.883333333333333)
    116.76666666666667

_YKS_ ::
  VFR_Waypoint
_YKS_ =
  VFR_Waypoint
    "YORKEYS KNOB"
    (Just "QLD")
    "YKS"
    (-16.81)
    145.725

_ORNN_ ::
  VFR_Waypoint
_ORNN_ =
  VFR_Waypoint
    "YORNANING TOWNSHIP"
    (Just "WA")
    "ORNN"
    (-32.733333333333334)
    117.16666666666667

_ULAB_ ::
  VFR_Waypoint
_ULAB_ =
  VFR_Waypoint
    "YULABILLA"
    (Just "QLD")
    "ULAB"
    (-27.066666666666666)
    149.7

_URAR_ ::
  VFR_Waypoint
_URAR_ =
  VFR_Waypoint
    "YURARABA"
    (Just "QLD")
    "URAR"
    (-28.333333333333332)
    151.4

_ZIN_ ::
  VFR_Waypoint
_ZIN_ =
  VFR_Waypoint
    "ZUIZIN ISLAND"
    (Just "QLD")
    "ZIN"
    (-10.1)
    143.33333333333334

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
