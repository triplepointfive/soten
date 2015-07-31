-- | Implement shared utility functions for postprocessing steps.
module Codec.Soten.PostProcess (
    PostProcessStep(..)
  , convertToLeftHanded
  , targetRealtimeFast
  , targetRealtimeQuality
  , targetRealtimeMaxQuality
) where

import Codec.Soten.PostProcess.FindInvalidData as FindInvalidData
import Codec.Soten.Scene

-- | Flags for possible post processing steps.
data PostProcessStep
      -- | Calculates the tangents and bitangents for the imported meshes.
    = CalcTangentSpace
      -- | Identifies and joins identical vertex data sets within all
      -- imported meshes.
    | JoinIdenticalVertices
      -- | Converts all the imported data to a left-handed coordinate space.
    | MakeLeftHanded
      -- | Triangulates all faces of all meshes.
    | Triangulate
      -- | Removes some parts of the data structure (animations, materials,
      -- light sources, cameras, textures, vertex components).
    | RemoveComponent
      -- | Generates normals for all faces of all meshes.
    | GenNormals
      -- | Generates smooth normals for all vertices in the mesh.
    | GenSmoothNormals
      -- | Splits large meshes into smaller sub-meshes.
    | SplitLargeMeshes
      -- | Removes the node graph and pre-transforms all vertices with
      -- the local transformation matrices of their nodes.
    | PreTransformVertices
      -- | Limits the number of bones simultaneously affecting a single vertex
      -- to a maximum value.
    | LimitBoneWeights
      -- | Validates the imported scene data structure.
    | ValidateDataStructure
      -- | Reorders triangles for better vertex cache locality.
    | ImproveCacheLocality
      -- | Searches for redundant/unreferenced materials and removes them.
    | RemoveRedundantMaterials
      -- | This step tries to determine which meshes have normal vectors
      -- that are facing inwards and inverts them.
    | FixInfacingNormals
      -- | This step splits meshes with more than one primitive type in
      -- homogeneous sub-meshes.
    | SortByPType
      -- | This step searches all meshes for degenerate primitives and
      -- converts them to proper lines or points.
    | FindDegenerates
      -- | This step searches all meshes for invalid data, such as zeroed
      -- normal vectors or invalid UV coords and removes/fixes them. This is
      -- intended to get rid of some common exporter errors.
    | FindInvalidData
      -- | This step converts non-UV mappings (such as spherical or
      -- cylindrical mapping) to proper texture coordinate channels.
    | GenUVCoords
      -- | This step applies per-texture UV transformations and bakes
      -- them into stand-alone vtexture coordinate channels.
    | TransformUVCoords
      -- | This step searches for duplicate meshes and replaces them
      -- with references to the first mesh.
    | FindInstances
      -- | A postprocessing step to reduce the number of meshes.
    | OptimizeMeshes
      -- | A postprocessing step to optimize the scene hierarchy.
    | OptimizeGraph
      -- | This step flips all UV coordinates along the y-axis and adjusts
      -- material settings and bitangents accordingly.
    | FlipUVs
      -- | This step adjusts the output face winding order to be CW.
    | FlipWindingOrder
      -- | This step splits meshes with many bones into sub-meshes so that each
      -- su-bmesh has fewer or as many bones as a given limit.
    | SplitByBoneCount
      -- | This step removes bones losslessly or according to some threshold.
    | Debone
    deriving (Show, Eq)

-- | Applies a post process on scene.
applyPostProcess :: Scene -> PostProcessStep -> Scene
applyPostProcess scene FindInvalidData = FindInvalidData.apply scene
applyPostProcess scene _ = scene

-- | Shortcut flag for Direct3D-based applications.
convertToLeftHanded :: [PostProcessStep]
convertToLeftHanded =
    [ MakeLeftHanded
    , FlipUVs
    , FlipWindingOrder
    ]

-- | Default postprocess configuration optimizing the data for real-time
-- rendering.
targetRealtimeFast :: [PostProcessStep]
targetRealtimeFast =
    [ CalcTangentSpace
    , GenNormals
    , JoinIdenticalVertices
    , Triangulate
    , GenUVCoords
    , SortByPType
    ]

-- | Default postprocess configuration optimizing the data for real-time
-- rendering.
targetRealtimeQuality :: [PostProcessStep]
targetRealtimeQuality =
    [ CalcTangentSpace
    , GenSmoothNormals
    , JoinIdenticalVertices
    , ImproveCacheLocality
    , LimitBoneWeights
    , RemoveRedundantMaterials
    , SplitLargeMeshes
    , Triangulate
    , GenUVCoords
    , SortByPType
    , FindDegenerates
    , FindInvalidData
    ]

-- | Default postprocess configuration optimizing the data for real-time
-- rendering.
targetRealtimeMaxQuality :: [PostProcessStep]
targetRealtimeMaxQuality = targetRealtimeQuality ++
    [ FindInstances
    , ValidateDataStructure
    , OptimizeMeshes
    ]
