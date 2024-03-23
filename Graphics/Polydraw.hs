{-# LANGUAGE FlexibleInstances #-}

 module Graphics.Polydraw (
     Model, Vector, Model2d, Model3d,
     cube, prismoid, pyramid, box, cylinder,
     union, difference, stack,
     rotate, translate, up,
     Chord, Facet,
     fa, fs, fn,
     r, d,
     draw, write, render) where

import Text.Printf
import Linear.V2
import Linear.V3
import Data.List hiding (union)

-- Returns if x is an int to n decimal places
isInt :: (RealFrac a) => Int -> a -> Bool
isInt n x = round (10.0 ^ n * (x - fromIntegral (round x :: Int))) == (0 :: Int)

fromV2 :: V2 Double -> V3 Double
fromV2 (V2 x y) = V3 x y 0

-- Common class for V2 and V3
class Vector a where
    renderVector :: a -> String

class Mesh a where
    meshHeight :: a -> Double

data Facet = Fa !Double | Fs !Double | Fn !Int | Def deriving (Show)

fa :: Double -> Facet
fa = Fa

fs :: Double -> Facet
fs = Fs

fn :: Int -> Facet
fn = Fn

data Chord = R !Double | D !Double deriving (Show)

r :: Double -> Chord
r = R

d :: Double -> Chord
d = D

data Solid =
    Cube !Double
  | Cylinder !Double !Chord !Facet
  | Prismoid ![Double] ![Double] !Double
  | Box !Double !Double !Double
  | ToSolid !Model2d
    deriving Show

data Model m =
    Solid !Solid
  | Translate !m !(Model m)
  | Rotate !m !(Model m)
  | Up !Double !(Model m)
  | Union ![Model m]
  | Stack ![Model m]
  | Difference !(Model m) ![Model m]
    deriving Show

type Model3d = Model (V3 Double)
type Model2d = Model (V2 Double)

-- | Turn a 'Model2d' into a 'Model3d' exactly as is.
solid :: Model2d -> Model3d
solid = Solid . ToSolid

cube :: Double -> Model3d
cube s = translate (fromV2 $ V2 (-s / 2) (-s / 2) ) (Solid $ Cube s)

box :: Double -> Double -> Double -> Model3d
box w h d = translate (fromV2 $ V2 (-w / 2) (-h / 2) ) (Solid $ Box w h d)

cylinder :: Double -> Chord -> Facet -> Model3d
cylinder h r f = Solid $ Cylinder h r f

prismoid :: [Double] -> [Double] -> Double -> Model3d
prismoid s1 s2 h = Solid $ Prismoid s1 s2 h

pyramid :: Double -> Double -> Double -> Model3d
pyramid s1 s2 h = Solid $ Prismoid [s1, s1] [s2, s2] h

translate :: Vector v => v -> Model v -> Model v
translate = Translate

rotate :: Vector v => v -> Model v -> Model v
rotate = Rotate

up :: Vector v => Double -> Model v -> Model v
up = Up

union :: Vector v => [Model v] -> Model v
union = Union

stack :: Vector v => [Model v] -> Model v
stack = Stack

difference :: Vector v => Model v -> [Model v] -> Model v
difference = Difference

renderDouble :: Double -> String
renderDouble n
  | isInt precision n = printf "%d" (floor n :: Int)
  | otherwise         = printf ("%." ++ show precision ++ "f") n
    where precision   = 3

renderVector' :: [Double] -> String
renderVector' v = "[" ++
    intercalate ", " (map renderDouble v)
    ++ "]"

instance Vector (V2 Double) where
    renderVector (V2 x y) = renderVector' [x, y]

instance Vector (V3 Double) where
    renderVector (V3 x y z) = renderVector' [x, y, z]

instance Mesh (V2 Double) where
    meshHeight (V2 _ _) = 0

instance Mesh (V3 Double) where
    meshHeight (V3 _ _ z) = z

instance Mesh Solid where
    meshHeight (Cube s)         = s
    meshHeight (Cylinder h _ _) = h
    meshHeight (Box _ h _)      = h
    meshHeight (Prismoid _ _ h) = h
    meshHeight (ToSolid _ )     = 0

instance Mesh (Model m) where
    meshHeight (Translate _ _)  = 0
    meshHeight (Rotate _ _)     = 0
    meshHeight (Up _ _)         = 0
    meshHeight (Union _)        = 0
    meshHeight (Stack _)        = 0
    meshHeight (Difference _ _) = 0 -- TODO: How to handle better - this is a silent failure that is hard to debug
    meshHeight (Solid s)        = meshHeight s

render :: Vector v => Model v -> String
render = renderModel

renderModel :: Vector v => Model v -> String
renderModel (Translate v s)   = renderTransform "translate" v s
renderModel (Rotate v s)      = renderTransform "rotate" v s
renderModel (Up h s)          = renderTransform "translate" (V3 0.0 0.0 h) s
renderModel (Solid s)         = renderSolid s
renderModel (Union xs)        = renderList "union()" xs
renderModel (Difference s xs) = renderList "difference()" (s:xs)
renderModel (Stack s)         = printf "union() {\n%s}\n" (renderStack s 0 "")
    where
        renderStack :: Vector v => [Model v] -> Double -> String -> String
        renderStack [] _ acc     = acc
        renderStack (x:xs) 0 acc = renderStack xs (meshHeight x)     (acc ++ printf "\t%s" (renderModel x))
        renderStack (x:xs) h acc = renderStack xs (h + meshHeight x) (acc ++ printf "\t%s" (renderModel shifted))
            where
                shifted = up h x

renderList :: Vector v => String -> [Model v] -> String
renderList tName xs = printf "%s {\n\t%s}\n" tName body
    where body = intercalate "\t" (map renderModel xs)

renderFacet :: Facet -> String
renderFacet (Fa f) = printf "$fa = %s" (renderDouble f)
renderFacet (Fs f) = printf "$fs = %s" (renderDouble f)
renderFacet (Fn n) = printf "$fn = %s" (renderDouble $ fromIntegral n)
renderFacet Def    = ""

renderTransform :: (Vector a, Vector m) => String -> a -> Model m -> String
renderTransform tName v model = printf "%s(%s) %s" tName (renderVector v) (renderModel model)

renderSolid :: Solid -> String
renderSolid (Cube s)           = printf "cube(%s);\n" (renderDouble s)
renderSolid (Cylinder h (R r) f)   = printf "cylinder(h = %s, r = %s, %s);\n" (renderDouble h) (renderDouble r) (renderFacet f)
renderSolid (Cylinder h (D d) f)   = printf "cylinder(h = %s, d = %s, %s);\n" (renderDouble h) (renderDouble d) (renderFacet f)
renderSolid (Box w h d)        = printf "cube(%s);\n" (renderVector' [w, h, d])
renderSolid (Prismoid s1 s2 h) = printf "prismoid(%s, %s, %s);\n" (renderVector' s1) (renderVector' s2) (renderDouble h)
renderSolid (ToSolid _)        = ""

draw :: Vector v => Model v -> String
draw m = "include <BOSL2/std.scad>\n\n" ++ renderModel m

write :: Vector v => String -> Model v -> IO ()
write fOut m = writeFile fOut $ draw m
