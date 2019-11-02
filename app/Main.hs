{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import qualified RIO.Text as T
import Options.Applicative.Simple
import qualified Paths_vban_test

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_vban_test.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strOption ( long "listen"
                    <> short 'l'
                    <> help "Listen address"
                    <> value (T.pack "0.0.0.0:6900")
                    <> showDefault
                     )
       <*> strOption ( long "remote"
                    <> short 'r'
                    <> help "Remote address"
                     )
       <*> option auto ( long "gain"
                      <> short 'g'
                      <> help "Gain per sample [dB]"
                      <> value 0.0001
                      <> showDefault
                       )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
