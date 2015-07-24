{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.Maybe
import System.Exit

import qualified Data.Vector as V
import Graphics.UI.GLUT as GL

import Codec.Soten as Soten

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
        materialShininess FrontAndBack $= realToFrac shininess
    setProperties _ = return ()

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    flush

render :: Scene -> Node -> IO ()
render scene@Scene{..} Node{..} =
    preservingMatrix $ do
        when (isJust _nodeTransformation) $
            let mat = transpose $ fromJust _nodeTransformation
                matList = map realToFrac $ matToList mat :: [GLfloat]
            in do
              glMat <- newMatrix RowMajor matList :: IO (GLmatrix GLfloat)
              multMatrix glMat

        V.mapM_ renderMesh (V.map (_sceneMeshes V.!) _nodeMeshes)
        V.mapM_ (render scene) _nodeChildren
  where
    matToList :: M44 a -> [a]
    matToList (V4 (V4 a11 a12 a13 a14)
                  (V4 a21 a22 a23 a24)
                  (V4 a31 a32 a33 a34)
                  (V4 a41 a42 a43 a44))
        = [ a11, a12, a13, a14
          , a21, a22, a23, a24
          , a31, a32, a33, a34
          , a41, a42, a43, a44
          ]
    renderMesh :: Mesh -> IO ()
    renderMesh Mesh{..} = do
        when (isJust _meshMaterialIndex) $
            applyMaterial (_sceneMaterials V.! (fromJust _meshMaterialIndex))
        if V.null _meshNormals
           then lighting $= Disabled
           else lighting $= Enabled
        V.mapM_ renderFace _meshFaces
      where
        renderFace :: Soten.Face -> IO ()
        renderFace Face{..} = renderPrimitive faceMode $ do
            V.mapM_ renderVertex _faceIndices
          where
            faceMode = case V.length _faceIndices of
                1 -> Points
                2 -> Lines
                3 -> Triangles
                _ -> Polygon
            renderVertex :: Int -> IO ()
            renderVertex index = do
                when (isJust color) (GL.color (v4ToColor (fromJust color)))
                when (isJust normal) (GL.normal (v3ToNormal (fromJust normal)))
                vertex (v3ToVertex (_meshVertices V.! index))
              where
                color  = _meshColors V.!? index
                normal = _meshNormals V.!? index

v3ToVertex :: V3 Float -> Vertex3 GLfloat
v3ToVertex (V3 x y z) = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

v3ToNormal :: V3 Float -> Normal3 GLfloat
v3ToNormal (V3 x y z) = Normal3 (realToFrac x) (realToFrac y) (realToFrac z)

v3ToColor :: V3 Float -> Color4 GLfloat
v3ToColor (V3 x y z) = Color4 (realToFrac x) (realToFrac y) (realToFrac z) 1

v4ToColor :: V4 Float -> Color4 GLfloat
v4ToColor (V4 x y z a) =
    Color4 (realToFrac x) (realToFrac y) (realToFrac z) (realToFrac a)
