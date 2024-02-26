{-# LANGUAGE FlexibleInstances #-}

 module Graphics.Polydraw where

import Text.Printf
import Linear.V2
import Linear.V3

-- Common class for V2 and V3
class Vector a where
    render :: a -> String

instance Vector (V2 Float) where
    render (V2 x y) = printf "[%.3f, %.3f]" x y

instance Vector (V2 Double) where
    render (V2 x y) = printf "[%.3f, %.3f]" x y

instance Vector (V2 Integer) where
    render (V2 x y) = printf "[%d, %d]" x y

instance Vector (V3 Float) where
    render (V3 x y z) = printf "[%.3f, %.3f, %.3f]" x y z

instance Vector (V3 Double) where
    render (V3 x y z) = printf "[%.3f, %.3f, %.3f]" x y z

instance Vector (V3 Integer) where
    render (V3 x y z) = printf "[%d, %d, %d]" x y z


data Solid a =
    Cube a
  | Cylinder a a
    deriving (Eq, Ord, Show, Read)
