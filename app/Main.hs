module Main where

import Control.Concurrent
import System.MIDI
import System.MIDI.Base
import System.MIDI.Utility
import System.Environment
import Sequence

main :: IO ()
main = do
  args  <- getArgs
  file  <- return $ args !! (length args - 1)

  outputDevice <- selectOutputDevice "output device:" $ Just "SimpleSynth virtual input"
  putStrLn "press enter to exit."

  output <- openDestination outputDevice
  start output

  thread <- forkIO (sequencer output 0 file)
  getLine

  killThread thread
  close output
