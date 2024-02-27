{-# LANGUAGE FlexibleInstances #-}

 module Graphics.Polydraw (
     Model, Vector,
     cube,
     union, difference,
     translate, up,
     render) where

import Text.Printf
import Linear.V2
import Linear.V3
import Data.List hiding (union)

-- Returns if x is an int to n decimal places
isInt :: (Integral a, RealFrac b) => a -> b -> Bool
isInt n x = round (10 ^ fromIntegral n * (x - fromIntegral (round x))) == 0

fromV2 :: V2 Double -> V3 Double
fromV2 (V2 x y) = V3 x y 0

up :: Double -> Model3d -> Model3d
up z = translate (V3 0 0 z)

-- Common class for V2 and V3
class Vector a where
    renderVector :: a -> String

data Solid =
    Cube Double
  | Cylinder Double Double
    deriving Show

data Model m =
    Solid Solid
  | Translate m (Model m)
  | Union [Model m]
  | Difference (Model m) [Model m]
    deriving Show

type Model3d = Model (V3 Double)

cube :: Double -> Model3d
cube s = translate (fromV2 $ V2 (-s/2) (-s/2) ) (Solid $ Cube s)

translate :: Vector v => v -> Model v -> Model v
translate = Translate

union :: Vector v => [Model v] -> Model v
union = Union

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

render :: Vector v => Model v -> String
render = renderModel

renderModel :: Vector v => Model v -> String
renderModel (Translate v s)   = renderTransform "translate" v s
renderModel (Solid s)         = renderSolid s
renderModel (Union xs)        = renderList "union()" xs
renderModel (Difference s xs) = renderList "difference()" (s:xs)

renderList :: Vector v => String -> [Model v] -> String
renderList tName xs = printf "%s {\n\t%s}" tName body
    where body = intercalate "\t" (map renderModel xs)

renderTransform :: (Vector a, Vector m) => String -> a -> Model m -> String
renderTransform tName v model = printf "%s(%s) %s" tName (renderVector v) (renderModel model)

renderSolid :: Solid -> String
renderSolid (Cube s) = printf "cube(%s);\n" (renderDouble s)
renderSolid _        = undefined
