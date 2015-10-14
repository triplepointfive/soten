{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad
import Data.Maybe
import Data.IORef
import System.Exit

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import qualified Data.Vector as V
import Graphics.Rendering.OpenGL as GL

import Codec.Soten as Soten

data BoundingBox =
    BoundingBox
    { sceneMin    :: !(V3 Float)
    , sceneMax    :: !(V3 Float)
    , sceneCenter :: !(V3 Float)
    } deriving Show

--
fieldOfView = 45.0

animationWaitTime = 3

main :: IO ()
main = do
  Gtk.initGUI

  -- Initialise the Gtk+ OpenGL extension
  -- (including reading various command line parameters)
  GtkGL.initGL

  -- We need a OpenGL frame buffer configuration to be able to create other
  -- OpenGL objects.
  glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                 GtkGL.GLModeDepth,
                                 GtkGL.GLModeDouble]

  -- Create an OpenGL drawing area widget
  canvas <- GtkGL.glDrawingAreaNew glconfig

  Gtk.widgetSetSizeRequest canvas 250 250

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)
  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \ window -> do
    (width, height) <- GtkGL.glDrawableGetSize window
    let aspectRatio = fromIntegral width / fromIntegral height

    clearColor $= Color4 0.0 0.0 0.0 0.0
    matrixMode $= Projection
    loadIdentity
    -- GL.ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    GL.perspective fieldOfView aspectRatio 1.0 1000.0
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    depthFunc $= Just Less
    drawBuffer $= BackBuffers


  -- model <- readModelFile "../models/stl/block_ascii.stl"
  model <- readModelFile "../models/md2/phoenix_ugv.md2"
  case model of
      Right scene -> do
          clearColor         $= Color4 0.1 0.1 0.1 1
          lighting           $= Enabled
          light (GL.Light 0) $= Enabled -- Uses default lighting parameters.
          depthFunc          $= Just Lequal
          lightModelTwoSide  $= Enabled
          GL.normalize       $= Enabled
          colorMaterial      $= Just (FrontAndBack, Diffuse)

          -- elapsedTime

          sceneRef <- newIORef scene
          angleRef <- newIORef 45.0
          -- idleCallback    $= Just (idle angleRef)
          -- displayCallback $=


          -- Set the repaint handler
          Gtk.onExpose canvas $ \_ -> do
            GtkGL.withGLDrawingArea canvas $ \glwindow -> do
              GL.clear [GL.DepthBuffer, GL.ColorBuffer]
              -- display
              display' angleRef sceneRef (boundingBox scene)
              GtkGL.glDrawableSwapBuffers glwindow
            return True

          -- Setup the animation
          Gtk.timeoutAddFull (do
              Gtk.widgetQueueDraw canvas
              angleRef $~! (+ 1.01)
              return True)
            Gtk.priorityDefaultIdle animationWaitTime

          --------------------------------
          -- Setup the rest of the GUI:
          --
          window <- Gtk.windowNew
          Gtk.onDestroy window Gtk.mainQuit
          Gtk.set window [ Gtk.containerBorderWidth := 8,
                           Gtk.windowTitle := "3D models viewer" ]

          vbox <- Gtk.vBoxNew False 4
          Gtk.set window [ Gtk.containerChild := vbox ]

          label <- Gtk.labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
          button <- Gtk.buttonNewWithLabel "Close"
          Gtk.onClicked button Gtk.mainQuit
          Gtk.set vbox [ Gtk.containerChild := canvas,
                         Gtk.containerChild := label,
                         Gtk.containerChild := button ]

          Gtk.widgetShowAll window
          Gtk.mainGUI
      Left msg -> do
          putStrLn msg
          exitFailure


-- Draw the OpenGL polygon.
display :: IO ()
display = do
  loadIdentity
  color (Color3 1 1 1 :: Color3 GLfloat)
  -- Instead of glBegin ... glEnd there is renderPrimitive.
  renderPrimitive Polygon $ do
    vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)

display' :: IORef GLfloat -> IORef Scene -> BoundingBox -> IO ()
display' angleRef sceneRef BoundingBox{..} = do
    matrixMode $= Modelview 0
    loadIdentity
    GL.lookAt (Vertex3 0 0 3) (Vertex3 0 0 (-5)) (Vector3 0 1 0)

    -- Rotate it around the y axis.
    scale scaleKoef scaleKoef scaleKoef
    angle <- readIORef angleRef
    GL.rotate angle (Vector3 0 1 0 :: Vector3 GLfloat)
    translate (v3ToVector $ fmap ((*(-1)) . realToFrac) sceneCenter)

    scene <- readIORef sceneRef
    render scene (_sceneRootNode scene)
  where
    scaleKoef = realToFrac $ 1 / maximum [tX, tY, tZ] :: GLfloat
      where
        (V3 tX tY tZ) = sceneMax - sceneMin

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
            applyMaterial (_sceneMaterials V.! fromJust _meshMaterialIndex)
        if V.null _meshNormals
           then lighting $= Disabled
           else lighting $= Enabled
        V.mapM_ renderFace _meshFaces
      where
        renderFace :: Soten.Face -> IO ()
        renderFace Face{..} = renderPrimitive faceMode $
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

boundingBox :: Scene -> BoundingBox
boundingBox Scene{..} =
    BoundingBox
    { sceneMin    = minBound
    , sceneMax    = maxBound
    , sceneCenter = (minBound + maxBound) / 2
    }
  where
    initBounds = (V3 1e10 1e10 1e10, V3 (-1e10) (-1e10) (-1e10))
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