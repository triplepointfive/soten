{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import System.Exit

import qualified Data.Vector as V
import Graphics.UI.GLUT as GL

import Codec.Soten

data BoundingBox =
    BoundingBox
    { sceneMin    :: !(V3 Float)
    , sceneMax    :: !(V3 Float)
    , sceneCenter :: !(V3 Float)
    } deriving Show

main :: IO ()
main = do
    (_, args) <- getArgsAndInitialize

    initialDisplayMode $= [RGBMode, DoubleBuffered, WithDepthBuffer]
    initialWindowSize $= Size 900 600
    initialWindowPosition $= Position 100 100

    createWindow "Soten - Very simple OpenGL sample"
    displayCallback $= display
    reshapeCallback $= Just reshape

    when (null args) exitFailure
    model <- readModelFile (head args)
    case model of
        Right scene -> do
            clearColor         $= Color4 0.1 0.1 0.1 1
            lighting           $= Enabled
            light (GL.Light 0) $= Enabled -- Uses default lighting parameters.
            depthFunc          $= Just Lequal
            lightModelTwoSide  $= Enabled
            GL.normalize       $= Enabled
            colorMaterial      $= Just (FrontAndBack, Diffuse)

            elapsedTime

            mainLoop
        Left msg -> do
            putStrLn msg
            exitFailure

boundingBox :: Scene -> BoundingBox
boundingBox Scene{..} =
    BoundingBox
    { sceneMin    = minBound
    , sceneMax    = maxBound
    , sceneCenter = (minBound + maxBound) / 2
    }
  where
    initBounds = ((V3 1e10 1e10 1e10), (V3 (-1e10) (-1e10) (-1e10)))
    (minBound, maxBound) = nodeBound identity initBounds _sceneRootNode

    nodeBound :: M44 Float -> (V3 Float, V3 Float) -> Node
              -> (V3 Float, V3 Float) -- ^ Min and max bounds.
    nodeBound trafo (minVec, maxVec) Node{..} =
        V.foldl (nodeBound mTrafo) (minV, maxV) _nodeChildren
      where
        mTrafo = maybe trafo (trafo !*!) _nodeTransformation
        (minV, maxV) = V.foldl meshBound (minVec, maxVec) nodeMeshes
        nodeMeshes = V.map (_sceneMeshes V.!) _nodeMeshes

        meshBound :: (V3 Float, V3 Float) -> Mesh -> (V3 Float, V3 Float)
        meshBound bounds Mesh{..} = V.foldl compareV bounds _meshVertices

    compareV :: (V3 Float, V3 Float) -> V3 Float -> (V3 Float, V3 Float)
    compareV (V3 minX minY minZ, V3 maxX maxY maxZ) (V3 tX tY tZ) =
        ( V3 (min minX tX) (min minY tY) (min minZ tZ)
        , V3 (max maxX tX) (max maxY tY) (max maxZ tZ)
        )

reshape :: ReshapeCallback
reshape (Size width height) = do
    matrixMode $= Projection
    loadIdentity
    GL.perspective fieldOfView aspectRatio 1.0 1000.0
    viewport $= (Position 0 0, Size width height)
  where
    aspectRatio = fromIntegral width / fromIntegral height
    fieldOfView = 45.0

-- | TODO: Fix me please
applyMaterial :: Material -> IO ()
applyMaterial (Material properties) = do
    materialDiffuse   FrontAndBack $= Color4 0.8 0.8 0.8 1
    materialSpecular  FrontAndBack $= Color4 0 0 0 1
    materialAmbient   FrontAndBack $= Color4 0.2 0.2 0.2 1
    materialEmission  FrontAndBack $= Color4 0 0 0 1
    materialShininess FrontAndBack $= 0

    polygonMode $= (Fill, Fill)
    cullFace $= Just Back

    mapM_ setProperties properties
  where
    setProperties :: MaterialProperty -> IO ()
    setProperties (MaterialColorDiffuse color) =
        materialDiffuse FrontAndBack $= v3ToColor color
    setProperties (MaterialColorSpecular color) =
        materialSpecular FrontAndBack $= v3ToColor color
    setProperties (MaterialColorAmbient color) =
        materialAmbient FrontAndBack $= v3ToColor color
    setProperties (MaterialColorEmissive color) =
        materialEmission FrontAndBack $= v3ToColor color
    setProperties (MaterialWireframe True) =
        polygonMode $= (Line, Line)
    setProperties (MaterialTwosided True) =
        cullFace $= Nothing
    -- TODO: Check for shininess strength.
    setProperties (MaterialColorShininess shininess) =
        materialShininess FrontAndBack $= shininess

    v3ToColor :: V3 Float -> Color4 GLfloat
    v3ToColor (V3 x y z) = Color4 (realToFrac x) (realToFrac y) (realToFrac z) 1

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    flush
