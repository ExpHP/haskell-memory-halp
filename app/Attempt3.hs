{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

-- OH NO WONDER

import           "base" Control.Monad
import           "aeson" Data.Aeson
import qualified "bytestring" Data.ByteString as ByteString
import qualified "yaml" Data.Yaml as Yaml

import           SharedJunk

readYaml :: (FromJSON a)=> Prelude.FilePath -> IO a
readYaml = ByteString.readFile >=> either fail pure . Yaml.decodeEither

main :: IO ()
main = doMain $ do
    _ <- readYaml "band.yaml" :: IO Value
    pure ()
