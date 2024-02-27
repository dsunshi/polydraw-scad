{-# LANGUAGE FlexibleInstances #-}

 module Graphics.Polydraw where

import Text.Printf
import Linear.V2
import Linear.V3

-- Returns if x is an int to n decimal places
isInt :: (Integral a, RealFrac b) => a -> b -> Bool
isInt n x = round (10 ^ fromIntegral n * (x - fromIntegral (round x))) == 0

renderDouble :: Double -> String
renderDouble n
  | isInt precision n = printf "%d" (floor n :: Int)
  | otherwise         = printf ("%." ++ show precision ++ "f") n
    where precision   = 3

fromV2 :: V2 Double -> V3 Double
fromV2 (V2 x y) = V3 x y 0

-- Common class for V2 and V3
class Vector a where
    renderVector :: a -> String

instance Vector (V2 Double) where
    renderVector (V2 x y) = printf "[%s, %s]" (renderDouble x) (renderDouble y)

instance Vector (V3 Double) where
    renderVector (V3 x y z) = printf "[%s, %s, %s]" (renderDouble x) (renderDouble y) (renderDouble z)

data Solid =
    Cube Double
  | Cylinder Double Double
    deriving Show

data Model m =
    Solid Solid
  | Translate m (Model m)
    deriving Show

type Model3d = Model (V3 Double)

cube :: Double -> Model3d
cube s = translate (fromV2 $ V2 (-s/2) (-s/2) ) (Solid $ Cube s)

translate :: Vector v => v -> Model v -> Model v
translate = Translate

renderModel :: Vector v => Model v -> String
renderModel (Translate v s) = renderTransform "translate" v s
renderModel (Solid s)       = renderSolid s

renderTransform :: (Vector a, Vector m) => String -> a -> Model m -> String
renderTransform tName v model = printf "%s(%s) %s" tName (renderVector v) (renderModel model)

renderSolid :: Solid -> String
renderSolid (Cube s) = printf "cube(%s);\n" (show s)
renderSolid _        = undefined
