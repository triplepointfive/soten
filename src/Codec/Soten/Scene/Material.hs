{-# LANGUAGE TemplateHaskell #-}
module Codec.Soten.Scene.Material (
    TextureOp(..)
  , TextureMapMode(..)
  , TextureMapping(..)
  , TextureType(..)
  , ShadingMode(..)
  , TextureFlags(..)
  , BlendMode(..)

  , UVTransform(..)
  , uvTransformTranslation
  , uvTransformScaling
  , uvRotation
  , newUVTransform

  , MaterialProperty(..)
  , addProperty

  , Material(..)
  , materialProperties
  , newMaterial
) where

import           Control.Lens (makeLenses, (&), (%~))
import qualified Data.Vector as V
import           Linear (V2(..))

import           Codec.Soten.Types (Color3D)

-- | Defines how the Nth texture of a specific type is combined with
-- the result of all previous layers.
data TextureOp
    -- | T = T1 * T2
    = TextureOpMultiply
    -- | T = T1 + T2
    | TextureOpAdd
    -- | T = T1 - T2
    | TextureOpSubtract
    -- | T = T1 / T2
    | TextureOpDivide
    deriving (Show, Eq)

-- | Defines how UV coordinates outside the [0...1] range are handled.
data TextureMapMode
    -- | A texture coordinate u|v is translated to u%1|v%1
    = TextureMapModeWrap
    -- | Texture coordinates outside [0...1] are clamped to the nearest valid
    -- value.
    | TextureMapModeClamp
    -- | If the texture coordinates for a pixel are outside [0...1] the texture
    -- is not applied to that pixel.
    | TextureMapModeDecal
    -- | A texture coordinate u|v becomes u%1|v%1 if (u-(u%1))%2 is zero and
    -- 1-(u%1)|1-(v%1) otherwise.
    | TextureMapModeMirror
    deriving (Show, Eq)

-- | Defines how the mapping coords for a texture are generated.
data TextureMapping
    -- | The mapping coordinates are taken from an UV channel.
    = TextureMappingUV
    -- | Spherical mapping.
    | TextureMappingSphere
    -- | Cylindrical mapping.
    | TextureMappingCylinder
    -- | Cubic mapping.
    | TextureMappingBox
    -- | Planar mapping.
    | TextureMappingPlane
    -- | Undefined mapping. Have fun.
    | TextureMappingOther
    deriving (Show, Eq)

-- | Defines the purpose of a texture
data TextureType
    -- | Dummy value.
    = TextureTypeNone
    -- | The texture is combined with the result of the diffuse lighting
    -- equation.
    | TextureTypeDiffuse
    -- | The texture is combined with the result of the specular lighting
    -- equation.
    | TextureTypeSpecular
    -- | The texture is combined with the result of the ambient lighting
    -- equation.
    | TextureTypeAmbient
    -- | The texture is added to the result of the lighting calculation.
    | TextureTypeEmissive
    -- | The texture is a height map.
    | TextureTypeHeight
    -- | The texture is a (tangent space) normal-map.
    | TextureTypeNormals
    -- | The texture defines the glossiness of the material.
    | TextureTypeShininess
    -- | The texture defines per-pixel opacity.
    | TextureTypeOpacity
    -- | Displacement texture.
    | TextureTypeDisplacement
    -- | Lightmap texture (aka Ambient Occlusion).
    | TextureTypeLightmap
    -- | Reflection texture.
    | TextureTypeReflection
    -- | Unknown texture.
    | TextureTypeUnknown
    deriving (Show, Eq)

-- | Defines all shading models supported by the library.
data ShadingMode
    -- | Flat shading. Shading is done on per-face base, diffuse only.
    = ShadingModeFlat
    -- | Simple Gouraud shading.
    | ShadingModeGouraud
    -- | Phong-Shading .
    | ShadingModePhong
    -- | Phong-Blinn-Shading.
    | ShadingModeBlinn
    -- | Toon-Shading per pixel.
    | ShadingModeToon
    -- | OrenNayar-Shading per pixel.
    | ShadingModeOrenNayar
    -- | Minnaert-Shading per pixel.
    | ShadingModeMinnaert
    -- | CookTorrance-Shading per pixel.
    | ShadingModeCookTorrance
    -- | Fresnel shading.
    | ShadingModeFresnel
    -- | No shading at all. Constant light influence of 1.0.
    | ShadingModeNoShading
    deriving (Show, Eq)

-- | Defines some mixed flags for a particular texture.
data TextureFlags
    -- | The texture's color values have to be inverted (componentwise 1-n).
    = TextureFlagsInvert
    -- | Explicit request to the application to process the alpha channel of the
    -- texture.
    | TextureFlagsUseAlpha
    -- | Explicit request to the application to ignore the alpha channel of the
    -- texture.
    | TextureFlagsIgnoreAlpha
    deriving (Show, Eq)

-- | Defines alpha-blend flags.
data BlendMode
    -- | SourceColor*SourceAlpha + DestColor*(1-SourceAlpha).
    = BlendModeDefault
    -- | SourceColor*1 + DestColor*1.
    | BlendModeAdditive

-- | Defines how an UV channel is transformed.
data UVTransform =
    UVTransform
    { -- | Translation on the u and v axes.
      _uvTransformTranslation :: !(V2 Float)
      -- | Scaling on the u and v axes.
    , _uvTransformScaling     :: !(V2 Float)
      -- | Rotation - in counter-clockwise direction.
    , _uvRotation             :: !Float
    }
    deriving Show
makeLenses ''UVTransform

newUVTransform :: UVTransform
newUVTransform =
    UVTransform
    { _uvTransformTranslation = V2 0 0
    , _uvTransformScaling     = V2 1 1
    , _uvRotation             = 0
    }

-- | Data structure for a single material property.
data MaterialProperty
    = MaterialName           !String
    | MaterialShadingModel   !ShadingMode
    | MaterialColorAmbient   !Color3D
    | MaterialColorDiffuse   !Color3D
    | MaterialColorSpecular  !Color3D
    | MaterialColorEmissive  !Color3D
    | MaterialColorShininess !Float
    | MaterialColorOpacity   !Float
    | MaterialRefracti       !Float
    | MaterialTexture        !TextureType !String
    | MaterialMappingModeU   !TextureType !TextureMapMode
    | MaterialMappingModeV   !TextureType !TextureMapMode
    | MaterialTwosided       !Bool
    | MaterialWireframe      !Bool
    deriving (Show, Eq)

-- | Data structure for a material.
data Material =
    Material
    { -- | List of all material properties loaded.
      _materialProperties :: !(V.Vector MaterialProperty)
    }
    deriving Show
makeLenses ''Material

-- | Adds a property to material.
addProperty :: Material -> MaterialProperty -> Material
addProperty mat property = mat & materialProperties  %~ V.cons property

newMaterial :: Material
newMaterial =
    Material
    { _materialProperties = V.empty
    }
