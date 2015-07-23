module Main where

import Control.Monad
import System.Exit

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
boundingBox scene =
    BoundingBox
    { sceneMin    = minBound
    , sceneMax    = maxBound
    , sceneCenter = centerBound
    }
  where
    (minBound, maxBound) =
        nodeBound
            (_sceneRootNode scene)
            (V3 1e10 1e10 1e10)
            (V3 (-1e10) (-1e10) (-1e10))
            identity
    centerBound = (minBound + maxBound) / 2

nodeBound :: Node -> V3 Float -> V3 Float -> M44 Float -> (V3 Float, V3 Float)
nodeBound node minVec maxVec trafo = undefined

reshape :: ReshapeCallback
reshape = undefined

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  flush
