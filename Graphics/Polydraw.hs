{-# LANGUAGE FlexibleInstances #-}

 module Graphics.Polydraw where

import Text.Printf
import Linear.V2
import Linear.V3

data Numm = Float | Double | Integer

-- Common class for V2 and V3
class Vector a where
    renderVector :: a -> String

instance Vector (V2 Float) where
    renderVector (V2 x y) = printf "[%.3f, %.3f]" x y

instance Vector (V2 Double) where
    renderVector (V2 x y) = printf "[%.3f, %.3f]" x y

instance Vector (V2 Integer) where
    renderVector (V2 x y) = printf "[%d, %d]" x y

instance Vector (V3 Float) where
    renderVector (V3 x y z) = printf "[%.3f, %.3f, %.3f]" x y z

instance Vector (V3 Double) where
    renderVector (V3 x y z) = printf "[%.3f, %.3f, %.3f]" x y z

instance Vector (V3 Integer) where
    renderVector (V3 x y z) = printf "[%d, %d, %d]" x y z


data Solid =
    Cube Rational
  | Cylinder Rational Rational
    deriving Show

data Model m =
    Solid Solid
  | Translate m (Model m)
    deriving Show

type Model3d = Model (V3 Rational)

cube :: Rational -> Model3d
cube = Solid . Cube

translate :: Vector v => v -> Model v -> Model v
translate = Translate

renderModel :: Vector v => Model v -> String
renderModel (Translate v s) = renderVectorSolid "translate" v s
renderModel (Solid s) = renderSolid s

renderVectorSolid :: (Vector a, Vector m) => String -> a -> Model m -> String
renderVectorSolid name vec solid = printf "%s(%s) %s" name (renderVector vec) (renderModel solid)

renderSolid :: Solid -> String
renderSolid (Cube s) = printf "cube(%.3f);\n" (show s)
