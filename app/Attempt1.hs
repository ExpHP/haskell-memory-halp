{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Raw types for parsing band.yaml,
-- with straightforward ToJSON/FromJSON implementations.

import           "base" Data.Complex
import           "base" Control.Applicative
import           "base" Control.Monad
import           "aeson" Data.Aeson
import           "aeson" Data.Aeson.TH
import           "vector" Data.Vector(Vector)
import qualified "vector" Data.Vector as Vector
import qualified "bytestring" Data.ByteString as ByteString
import qualified "yaml" Data.Yaml as Yaml

import           SharedJunk

---------------------------------------------------------

data DataBand = DataBand
    { bandFrequency :: Double
    , bandEigenvector :: Maybe (Vector (Vector (Double, Double)))
    } deriving (Eq, Show, Read)

$(let f "bandFrequency" = "frequency"
      f "bandEigenvector" = "eigenvector"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''DataBand)

---------------------------------------------------------

data SpectrumData = SpectrumData
    { spectrumQPosition :: [Double]
    , spectrumDistance :: Double
    , spectrumBand :: Vector DataBand
    } deriving (Eq, Show, Read)

$(let f "spectrumQPosition" = "q-position"
      f "spectrumDistance" = "distance"
      f "spectrumBand" = "band"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''SpectrumData)

---------------------------------------------------------

data Point = Point
    { pointSymbol :: String
    , pointCoordinates :: [Double]
    , pointMass :: Double
    } deriving (Eq, Show, Read)

$(let f "pointSymbol" = "symbol"
      f "pointCoordinates" = "coordinates"
      f "pointMass" = "mass"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''Point)

---------------------------------------------------------

data BandYaml = BandYaml
    { bandYamlNQPoint :: Int
    , bandYamlNPath   :: Int
    , bandYamlSegmentNQPoint :: [Int]
    , bandYamlReciprocalLattice :: [[Double]]
    , bandYamlNAtom :: Int
    , bandYamlLattice :: [[Double]]
    , bandYamlPoints :: Vector Point
    , bandYamlSupercellMatrix :: [[Double]]
    , bandYamlSpectrum :: Vector SpectrumData
    } deriving (Eq, Show, Read)

$(let f "bandYamlNQPoint"           = "nqpoint"
      f "bandYamlNPath"             = "npath"
      f "bandYamlSegmentNQPoint"    = "segment_nqpoint"
      f "bandYamlReciprocalLattice" = "reciprocal_lattice"
      f "bandYamlNAtom"             = "natom"
      f "bandYamlLattice"           = "lattice"
      f "bandYamlPoints"            = "points"
      f "bandYamlSupercellMatrix"   = "supercell_matrix"
      f "bandYamlSpectrum"          = "phonon"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''BandYaml)

---------------------------------------------------------

readYaml :: (FromJSON a)=> Prelude.FilePath -> IO a
readYaml = ByteString.readFile >=> either fail pure . Yaml.decodeEither

main :: IO ()
main = doMain $ do
    _ <- readYaml "band.yaml" :: IO BandYaml
    pure ()
