{-# LANGUAGE NoImplicitPrelude, QuasiQuotes #-}
{-# OPTIONS -Wall #-}
import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Lattice as Lattice
import qualified Algebra.Ring as Ring
import qualified Algebra.RealTranscendental as Trans
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy as BS       
import qualified Data.ByteString.Internal as BS       
import qualified Data.Foldable as F
import           Data.List (sort)
import           Data.Maybe (catMaybes)
import           Data.Tensor.TypeLevel
import           NumericPrelude
import           Prelude (Fractional,abs)
import           System.Process.QQ


---- String Functions
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p [] = []
splitBy p xs = a : (splitBy p $ dropWhile p $ b)
  where
    (a, b) = break p xs


----------------------------------------------------------------
---- Vector operators
----------------------------------------------------------------

infixl 7 .* -- scalar product
(.*) :: (Vector v, Ring.C a) => a -> v a -> v a
x .* ys = compose $ \i -> x * ys!i

infix 7 `dot` -- inner product
dot :: (Vector v, Ring.C a) => v a -> v a -> a
x `dot` y = contract $ \i -> x!i * y!i

infix 7 `cross` -- cross product
cross :: (Ring.C a) => Vec3 a -> Vec3 a -> Vec3 a
x `cross` y = compose $ \i -> 
  let 
    Axis n = i
    j = Axis (mod (n+1) 3)
    k = Axis (mod (n+2) 3)
    in x!j * y!k - x!k * y!j

norm :: (Vector v, Algebraic.C a) => v a -> a 
norm x = sqrt (dot x x)

normalize :: (Vector v, Algebraic.C a) => v a -> v a 
normalize x = (fromInteger 1 / norm x) .* x

----------------------------------------------------------------
---- Celestial Coordinate Systems
----------------------------------------------------------------
data Point a = 
  Point 
  { 
    coordinateSystem :: CoordinateSystem a, 
    longitude        :: a,     -- E-W direction
    latitude         :: a,     -- N-S direction
    toVector         :: Vec3 a -- a unit vector that points to that direction
  } deriving (Eq, Show)

data CoordinateSystem a =
  CoordinateSystem
  {
    label     :: String,
    northPole :: Point a,
    zeroPoint :: Point a
  } deriving (Eq, Show)

point :: (Trans.C a) => CoordinateSystem a -> a -> a -> Point a
point c lo la = Point c lo la $ normalize $ x.*ex + y.*ey + z.*ez
  where
    ex = toVector $ zeroPoint c
    ey = ez `cross` ex
    ez = toVector $ northPole c
    x  = cos lo * cos la
    y  = sin lo * cos la
    z  =          sin la

fromVector :: (Trans.C a) => CoordinateSystem a -> Vec3 a -> Point a
fromVector c v = transform c $ Point c{label="not "++label c} undefined undefined (normalize v)


transform :: (Trans.C a) => CoordinateSystem a -> Point a -> Point a
transform c p0@(Point c0 _ _ v) 
  | c == c0   = p0
  | otherwise = Point c lo la v
  where
    ex = toVector $ zeroPoint c
    ey = ez `cross` ex
    ez = toVector $ northPole c
    x = v `dot` ex 
    y = v `dot` ey
    z = v `dot` ez 
    r_xy = sqrt(sqr x + sqr y)
    lo = atan2 y x
    la = atan2 z r_xy

aitoffProjection :: (Fractional a, Trans.C a) => CoordinateSystem a -> Point a -> Maybe (Vec2 a)
aitoffProjection c p0 = Just $ Vec :~ x :~ y
  where
    Point{longitude = lo, latitude = la} = transform c p0
    sinc = sin alpha / alpha
    alpha = acos(cos la * cos (lo / 2))
    x = - 2 * cos la * sin (lo / 2) / sinc
    y = sin la / sinc

polarProjection :: (Fractional a, Trans.C a) => CoordinateSystem a -> Point a -> Maybe (Vec2 a)
polarProjection c p0 = (\x y -> Vec :~ x :~ y) <$> mx <*> my
  where
    Point{longitude = lo, latitude = la} = transform c p0
    mr 
      | la > -1e-10 = Just $ sqrt(1 - sin la)
      | otherwise   = Nothing
    mx = ((-sin lo) *) <$> mr
    my = ((-cos lo) *) <$> mr

dms :: (Fractional a, Trans.C a)  => a -> a -> a  -> a 
dms d m s = (d + (m + s/60)/60)/180 * pi

hms :: (Fractional a, Trans.C a)  => a -> a -> a  -> a 
hms h m s = (h + (m + s/60)/60)/12 * pi

equatorialSystem :: (Fractional a, Trans.C a) => CoordinateSystem a
equatorialSystem = 
  CoordinateSystem "equatorial" 
  (Point equatorialSystem 0 (pi/2) (Vec :~ 1 :~ 0 :~ 0))
  (Point equatorialSystem 0     0  (Vec :~ 0 :~ 1 :~ 0))

galacticSystem :: CoordinateSystem Double
galacticSystem = 
  CoordinateSystem "galactic"
  (point equatorialSystem (hms 12 51 26.282) ( dms 27 07 42.01))
  (point equatorialSystem (hms 17 45 37.224) (-dms 28 56 10.23))

superGalacticSystem :: CoordinateSystem Double
superGalacticSystem = 
  CoordinateSystem "supergalactic"
  (point galacticSystem (dms  47.37 0 0) (dms 6.32 0 0))
  (point galacticSystem (dms 137.37 0 0) (dms 0    0 0))


sas1, sas2 :: Point Double
sas1 = point equatorialSystem (hms  17 45 40.04) (-dms 29 0 28.1)
sas2 = point galacticSystem   (dms 359 56 39.5 ) (-dms  0 2 46.3)

projectIO :: String -> (Point Double -> Maybe (Vec2 Double)) -> IO ()
projectIO header proj = do
  fns <- fmap (lines . map BS.w2c . BS.unpack) [cmd| ls -1 data/*.txt |]
  stars <- fmap concat $ mapM (fmap parseConstellation . readFile) fns
  write (header ++ "-stars.txt") stars
  write (header ++ "-grid.txt") equatorialMesh
  write (header ++ "-galaxies.txt") superGalacticMesh
  where
    write fn pts = 
      writeFile fn $ 
        unlines $
        catMaybes $
        flip map pts $ \p -> 
        fmap (unwords . map show . F.toList) $ 
        proj p 

main :: IO ()
main = do
  projectIO "center"    $ aitoffProjection galacticSystem
  projectIO "frontier"  $ aitoffProjection $ 
    galacticSystem{zeroPoint = point galacticSystem pi 0}
  projectIO "northpole" $ polarProjection galacticSystem
  projectIO "southpole" $ polarProjection $
    galacticSystem{zeroPoint = point galacticSystem pi 0,
                   northPole = point galacticSystem 0 (-pi/2)}
    
parseConstellation :: String ->  [Point Double]
parseConstellation str = pts2
  where
    n = length pts
    pts2 = concat [interpolateL (pts!!i) (pts!!(mod (i+1) n))| i<-[0..n-1] ]

    pts :: [Point Double]
    pts = concat $ map (toPt . splitBy (=='|')) $ lines str

    toPt [str1, str2, _] = case map read $ words str1 of
      [h,m,s] -> [point equatorialSystem (hms h m s) (dms (read str2) 0 0)]
      _       -> []
    toPt _ = []


interpolateV :: (Fractional a, Trans.C a) => Point a -> Point a -> [Point a]
interpolateV p1@Point{toVector = v1} p2@Point{toVector = v2} 
  | norm (v1-v2) < 1e-3  = [p1]
  | otherwise            = interpolateV p1 mid ++ interpolateV mid p2
  where
    mid = fromVector (coordinateSystem p1) (v1+v2)

interpolateL :: Point Double -> Point Double -> [Point Double]
interpolateL p1@(Point c1 lo1 la1 v1)  p2@(Point _ lo2 la2 v2)
  | norm (v1-v2) < 5e-3  = [p1]
  | otherwise            = interpolateL p1 mid ++ interpolateL mid p2
  where
    mid = point c1 ((lo1+lo2')/2) ((la1+la2)/2)
    lo2' = snd  $ head $ sort [ (Prelude.abs (lo1-x), x) | x <- [lo2-2*pi, lo2,lo2+2*pi]]

equatorialMesh :: [Point Double]
equatorialMesh = 
  [point equatorialSystem lo la | lo <- lons , la <- lats2] ++ 
  [point equatorialSystem lo la | lo <- lons2, la <- lats ] 
  where 

    lonReso = 12
    latReso = 9

    lons = [ pi   * (fromInteger n/fromInteger lonReso) | n <- [ -lonReso ..lonReso-1]]    
    lats = [ pi/2 * (fromInteger n/fromInteger latReso) | n <- [1-latReso ..latReso-1]]

    lonReso2 = 72
    latReso2 = 90

    lons2 = [ pi   * (fromInteger n/fromInteger lonReso2) | n <- [ -lonReso2 ..lonReso2-1]]    
    lats2 = [ pi/2 * (fromInteger n/fromInteger latReso2) | n <- [1-latReso2 ..latReso2-1]]


superGalacticMesh :: [Point Double]
superGalacticMesh =
  [point superGalacticSystem (dms (fromInteger lo/10) 0 0) 0 | lo <- [0..3599]]
  