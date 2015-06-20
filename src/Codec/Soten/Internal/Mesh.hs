{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Internal.Mesh (
    PrimitiveType(..)

  , Face(..)
  , faceIndices

  , VertexWeight(..)
  , vertexWeightId    
  , vertexWeightWeight

  , Bone(..)
  , boneName
  , boneVertexWeights
  , boneOffsetMatrix

  , Mesh(..)
  , meshPrimitiveTypes
  , meshVertices
  , meshNormals
  , meshTangents
  , meshBitangents
  , meshColors
  , meshTextureCoords
  , meshFaces
  , meshBones
  , meshMaterialIndex
  , meshName
  , newMesh
  , hasPositions
  , hasFaces
  , hasNormals
  , hasTangentsAndBitangents
  , hasBones
  , hasVertexColors
  , hasTextureCoords

) where

import           Data.Maybe (isJust)

import           Control.Lens (makeLenses, (^.))
import qualified Data.Vector as V
import           Linear (V3(..))
import           Linear.Matrix (M44)

import           Codec.Soten.Types (Color4D, Index)

-- | The types of geometric primitives.
data PrimitiveType
      -- | A point primitive.
    = PrimitivePoint
      -- | A line primitive.
    | PrimitiveLine
      -- | A triangular primitive.
    | PrimitiveTriangle
      -- | A higher-level polygon with more than 3 edges.
    | PrimitivePolygone
    deriving (Show, Eq)

-- | A single face in a mesh, referring to multiple vertices.
data Face =
    Face
    { -- | The indices array.
      _faceIndices :: !(V.Vector Index)
    }
    deriving (Show, Eq)
makeLenses ''Face

-- | A single influence of a bone on a vertex.
data VertexWeight =
    VertexWeight
    { -- | Index of the vertex which is influenced by the bone.
      _vertexWeightId     :: !Index
      -- | The strength of the influence in the range (0...1).
    , _vertexWeightWeight :: !Float
    }
makeLenses ''VertexWeight

-- | A single bone of a mesh.
data Bone =
    Bone
    { -- | The name of the bone.
      _boneName :: !String
      -- | The vertices affected by this bone.
    , _boneVertexWeights :: !(V.Vector VertexWeight)
      -- | Matrix that transforms from mesh space to bone space in bind pose.
    , _boneOffsetMatrix :: !(M44 Float)
    }
makeLenses ''Bone

-- | A mesh represents a geometry or model with a single material.
data Mesh =
    Mesh
    { -- | This specifies which types of primitives are present in the mesh.
      _meshPrimitiveTypes :: !(V.Vector PrimitiveType)
      -- | Vertex positions.
    , _meshVertices       :: !(V.Vector (V3 Float))
      -- | Vertex normals.
    , _meshNormals        :: !(V.Vector (V3 Float))
      -- | Vertex tangents.
    , _meshTangents       :: !(V.Vector (V3 Float))
      -- | Vertex bitangents.
    , _meshBitangents     :: !(V.Vector (V3 Float))
      -- | Vertex color sets.
    , _meshColors         :: !(V.Vector Color4D)
      -- | Vertex texture coords, also known as UV channels.
    , _meshTextureCoords  :: !(V.Vector (V3 Float))
      -- | The faces the mesh is constructed from.
    , _meshFaces          :: !(V.Vector Face)
      -- | The bones of this mesh.
    , _meshBones          :: !(V.Vector Bone)
      -- | The material used by this mesh.
    , _meshMaterialIndex  :: !(Maybe Index)
      -- | Name of the mesh.
    , _meshName           :: !String
    }
makeLenses ''Mesh

newMesh :: Mesh
newMesh =
    Mesh
    { _meshPrimitiveTypes = V.empty
    , _meshVertices       = V.empty
    , _meshNormals        = V.empty
    , _meshTangents       = V.empty
    , _meshBitangents     = V.empty
    , _meshColors         = V.empty
    , _meshTextureCoords  = V.empty
    , _meshFaces          = V.empty
    , _meshBones          = V.empty
    , _meshMaterialIndex  = Nothing
    , _meshName           = ""
    }

-- | Check whether the mesh contains positions.
hasPositions :: Mesh -> Bool
hasPositions mesh = not (V.null (mesh ^. meshVertices))

-- | Check whether the mesh contains faces.
hasFaces :: Mesh -> Bool
hasFaces mesh = not (V.null (mesh ^. meshFaces))

-- | Check whether the mesh contains normal vectors.
hasNormals :: Mesh -> Bool
hasNormals mesh = not (V.null (mesh ^. meshNormals))

-- | Check whether the mesh contains tangent and bitangent vectors.
hasTangentsAndBitangents :: Mesh -> Bool
hasTangentsAndBitangents mesh =
    not (V.null (mesh ^. meshBitangents) || V.null (mesh ^. meshTangents))

-- | Check whether the mesh contains bones.
hasBones :: Mesh -> Bool
hasBones mesh = not (V.null (mesh ^. meshBones))

-- | Check whether the mesh contains a vertex color set.
hasVertexColors :: Mesh -> Index -> Bool
hasVertexColors mesh index = isJust ((mesh ^. meshColors) V.!? index)

-- | Check whether the mesh contains a texture coordinate set.
hasTextureCoords :: Mesh -> Index -> Bool
hasTextureCoords mesh index = isJust ((mesh ^. meshTextureCoords) V.!? index)
