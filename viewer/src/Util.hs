module Util where

import Linear
import Graphics.Rendering.OpenGL

v3ToVector :: V3 Float -> Vector3 GLfloat
v3ToVector (V3 x y z) = Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

v3ToVertex :: V3 Float -> Vertex3 GLfloat
v3ToVertex (V3 x y z) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

v3ToNormal :: V3 Float -> Normal3 GLfloat
v3ToNormal (V3 x y z) = Normal3 (realToFrac x) (realToFrac y) (realToFrac z)

v3ToColor :: V3 Float -> Color4 GLfloat
v3ToColor (V3 x y z) = Color4 (realToFrac x) (realToFrac y) (realToFrac z) 1

v4ToColor :: V4 Float -> Color4 GLfloat
v4ToColor (V4 x y z a) =
    Color4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac a)
