module Sequence where

import Control.Concurrent
import System.MIDI
import System.MIDI.Base
import System.MIDI.Utility
import Parse
import Eval

bpm :: Int
bpm = 120 -- TODO

sequencer :: Connection -> Int -> String -> IO ()
sequencer output n file = do
  input <- readFile file
  seqs  <- return $ eval $ parse input

  t <- currentTime output
  putStr $ "\nt: " ++ show t ++ ", notes on: "
  mapM (sequenceNth (noteOn output) n) seqs

  threadDelay $ truncate $ (60 / fromIntegral bpm) * (1000000 / 4)

  t <- currentTime output
  putStr $ "\nt: " ++ show t ++ ", notes off: "
  mapM (sequenceNth (noteOff output) n) seqs

  sequencer output (n + 1) file

sequenceNth :: (Int -> Int -> IO ()) -> Int -> ChanSeq -> IO ()
sequenceNth _ _ (_,[])  = return ()
sequenceNth f n (ch,xs) = mapM_ (f ch) (xs !! (n `mod` length xs))

noteOn :: Connection -> Int -> Int -> IO ()
noteOn output ch n = do
  putStr $ show n ++ " "
  send output $ MidiMessage ch $ NoteOn n 255

noteOff :: Connection -> Int -> Int -> IO ()
noteOff output ch n = do
  putStr $ show n ++ " "
  send output $ MidiMessage ch $ NoteOff n 255

