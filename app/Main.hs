{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Options.Applicative.Simple
import Relude
import System.FilePath ((-<.>), (<.>))

import Data.BULK
import Data.BULK.BARK qualified as BARK
import Data.BULK.Eval (mkContext)
import Data.BULK.Utils (failLeft)

main :: IO ()
main = do
    snd
        =<< simpleOptions
            "0.1"
            "BULK CLI"
            "Manipulate BULK data"
            (pure ())
            ( do
                addCommand
                    "untextify"
                    "convert a BULK text notation into a binary BULK stream"
                    untextify
                    (strArgument (metavar "FILE"))
                addSubCommands
                    "bark"
                    "Operations on BARK files"
                    ( do
                        addCommand
                            "manifest"
                            "check if manifest entries match the files"
                            manifest
                            (strArgument (metavar "FILE"))
                    )
            )

untextify :: FilePath -> IO ()
untextify file = do
    let target' = file -<.> "bulk"
        target = if file == target' then file <.> "bulk" else target'
    parseNotationFileBin file >>= either putStrLn (writeFileLBS target)

manifest :: FilePath -> IO ()
manifest file = do
    ctx <- loadNotationFiles (mkContext [BARK.hash0]) ["config/hash0.bulktext", "test/bulk/config/bark-alpha.bulktext"] >>= failLeft
    BARK.verifyManifest ctx file >>= either putStrLn (mapM_ $ putStrLn . BARK.displayVerification)
