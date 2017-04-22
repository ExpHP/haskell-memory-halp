{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- Nothing in this file matters with regards to the memory errors.
-- It just provides a common entry point for the 'attempts' in 'app/'
--   which parses arguments and creates an input file.

module SharedJunk where

import           "base" System.Environment
import           "base" System.IO
import           "base" Control.Monad
import           "bytestring" Data.ByteString.Char8(ByteString)
import qualified "bytestring" Data.ByteString.Char8 as ByteString

newtype NumEigenvectors = NumEigenvectors Int
newtype NumKPoints = NumKPoints Int
newtype NumAtoms = NumAtoms Int

-- let the user choose how much pain to inflict on their virtual address space
doMain realMain = getArgs >>= \case
    [] -> doMain' realMain (NumKPoints 900)
                           (NumEigenvectors 10)
                           (NumAtoms 300)
    [k,e,a] ->  doMain' realMain (NumKPoints $ read k)
                                 (NumEigenvectors $ read e)
                                 (NumAtoms $ read a)
    _ -> putStrLn "Usage: prog [NKPOINT NEIGENVECTOR NATOMS]"

doMain' :: IO a -> NumKPoints -> NumEigenvectors -> NumAtoms -> IO ()
doMain' f k e a = do
    putStrLn "Creating a fake input file..."
    prepareInputFile "band.yaml" k e a

    putStrLn "Reading input file..."
    f >> pure ()

prepareInputFile :: FilePath -> NumKPoints -> NumEigenvectors -> NumAtoms -> IO ()
prepareInputFile fp (NumKPoints n) e a = do
    ByteString.readFile "input-head.yaml" >>= ByteString.writeFile fp
    replicateM_ n $ appendKPoint fp e a

appendKPoint :: FilePath -> NumEigenvectors -> NumAtoms -> IO ()
appendKPoint fp (NumEigenvectors n) a = do
    let prefixText = ByteString.unlines kpointPrefixText
    let eigenText = ByteString.unlines $ eigenvectorText a
    ByteString.appendFile fp prefixText
    replicateM_ n $ ByteString.appendFile fp eigenText

eigenvectorText :: NumAtoms -> [ByteString]
eigenvectorText (NumAtoms n) =
    [ "  - # 12"
    , "    frequency:    0.2408721673"
    , "    eigenvector:"
    ] ++ concat (replicate n atomText)

atomText :: [ByteString]
atomText =
    [ "    - # atom 146"
    , "      - [  0.02181335176289, -0.06111582751302 ]"
    , "      - [  0.04176313398484,  0.03120140668575 ]"
    , "      - [ -0.00000157327902,  0.00000216002603 ]"
    ]

kpointPrefixText :: [ByteString]
kpointPrefixText =
    [ "- q-position: [    0.1515152,    0.0000000,    0.0000000 ]"
    , "  distance:    0.0000000"
    , "  band:"
    ]
