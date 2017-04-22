{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

import           "base" Data.Complex
import           "base" Control.Monad
import           "aeson" Data.Aeson
import           "aeson" Data.Aeson.Types
import           "vector" Data.Vector(Vector)
import           "text" Data.Text(Text)
import qualified "yaml" Data.Yaml as Yaml
import qualified "vector" Data.Vector as Vector
import qualified "bytestring" Data.ByteString as ByteString
import qualified "unordered-containers" Data.HashMap.Strict as HashMap

import           SharedJunk

---------------------------------------------------------
-- Helpers to deal with 'Parser -> Value a', a nested applicative.

type ParseFunc a = Value -> Parser a
infixl 4 <<$>>, <<*>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = (<$>) . (<$>)
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = (<*>) . fmap (<*>)
ppure :: (Applicative f, Applicative g) => a -> f (g a)
ppure = pure . pure

-- Now let's use the combinators!
-- (...let's combinate?)

parseComplex :: ParseFunc (Complex Double)
parseComplex = uncurry (:+) <<$>> parseJSON

parseVector :: ParseFunc a -> ParseFunc (Vector a)
parseVector f = withArray "parseVector" (Vector.mapM f)

onKey :: Text -> ParseFunc a -> ParseFunc a
onKey key p = withObject "parseKey" $ \o -> p (o HashMap.! key)

parseKet :: ParseFunc (Vector (Complex Double))
parseKet = onKey "eigenvector" $ -- Vector.convert .
  (>>= id) <<$>> parseVector (parseVector parseComplex)

parseKets :: ParseFunc (Vector (Vector (Complex Double)))
parseKets = onKey "band" $ parseVector parseKet

parseEigenvectors :: ParseFunc (Vector (Vector (Vector (Complex Double))))
parseEigenvectors = onKey "phonon" $ parseVector parseKets

---------------------------------------------------------

data Wrapper = Wrapper (Vector (Vector (Vector (Complex Double))))
instance FromJSON Wrapper where
    parseJSON = Wrapper <<$>> parseEigenvectors

readYaml :: (FromJSON a)=> Prelude.FilePath -> IO a
readYaml = ByteString.readFile >=> either fail pure . Yaml.decodeEither

main :: IO ()
main = doMain $ do
    _ <- readYaml "band.yaml" :: IO Wrapper
    pure ()
