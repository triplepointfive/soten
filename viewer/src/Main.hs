{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad
import Data.Maybe
import Data.IORef
import System.Exit

import qualified Data.Vector as V
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.UI.Gtk (AttrOp((:=)))
import Graphics.Rendering.OpenGL as GL

import Codec.Soten as Soten

import Bound
import Util

fieldOfView = 45.0
animationWaitTime = 3

main :: IO ()
main = do
    Gtk.initGUI
    GtkGL.initGL
    glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                   GtkGL.GLModeDepth,
                                   GtkGL.GLModeDouble]
    canvas <- GtkGL.glDrawingAreaNew glconfig
    Gtk.widgetSetSizeRequest canvas 500 500

    -- Initialise some GL setting just before the canvas first gets shown
    -- (We can't initialise these things earlier since the GL resources that
    -- we are using wouldn't heve been setup yet)
    Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \ window -> do
        (width, height) <- GtkGL.glDrawableGetSize window
        reshape width height
        depthFunc $= Just Less
        drawBuffer $= BackBuffers

    Gtk.onConfigure canvas $ \ (Gtk.Configure _ _ _ width height) ->
        reshape width height

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

            sceneRef <- newIORef scene
            angleRef <- newIORef 45.0


            -- Set the repaint handler
            Gtk.onExpose canvas $ \_ -> do
              GtkGL.withGLDrawingArea canvas $ \glwindow -> do
                GL.clear [GL.DepthBuffer, GL.ColorBuffer]
                display angleRef sceneRef (boundingBox scene)
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


            -- label <- Gtk.labelNew (Just "Gtk2Hs using OpenGL via HOpenGL!")
            -- button <- Gtk.buttonNewWithLabel "Close"
            -- Gtk.onClicked button Gtk.mainQuit
            vbox <- Gtk.vBoxNew False 4
            Gtk.set window [ Gtk.containerChild := vbox ]


            statusbar <- Gtk.statusbarNew
            cID <- Gtk.statusbarGetContextId statusbar ""
            Gtk.statusbarPush statusbar cID "Hello"

            panel <- Gtk.vBoxNew False 4

            hbox <- Gtk.hBoxNew False 4

            Gtk.set hbox [ Gtk.containerChild := canvas, Gtk.containerChild := panel ]

            -- Gtk.containerAdd window menu
            Gtk.set vbox [ -- Gtk.containerChild := menu,
                           Gtk.containerChild := hbox,
                           Gtk.containerChild := statusbar ]
                           -- Gtk.containerChild := button ]

            Gtk.widgetShowAll window
            Gtk.mainGUI
        Left msg -> putStrLn msg >> exitFailure

reshape :: (Num a, Integral a) => a -> a -> IO Bool
reshape width height = do
    clearColor $= Color4 0.0 0.0 0.0 0.0
    matrixMode $= Projection
    loadIdentity
    -- GL.ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
    GL.perspective fieldOfView aspectRatio 1.0 1000.0
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    return True
  where
    aspectRatio = fromIntegral width / fromIntegral height

display :: IORef GLfloat -> IORef Scene -> BoundingBox -> IO ()
display angleRef sceneRef BoundingBox{..} = do
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
