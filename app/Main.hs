{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Options.Applicative.Simple
import Relude
import System.FilePath ((-<.>), (<.>))

import Data.BULK

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
            )

untextify :: FilePath -> IO ()
untextify file = do
    let target' = file -<.> "bulk"
        target = if file == target' then file <.> "bulk" else target'
    parseNotationFileBin file >>= either putStrLn (writeFileLBS target)
