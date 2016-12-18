module Main where

import Data.List.Split (splitOn)
import Control.Concurrent (forkIO, killThread, threadDelay)
import System.MIDI (Connection, openDestination, start, stop, send, close, currentTime)
import System.MIDI.Base (MidiMessage (MidiMessage), MidiMessage' (NoteOn, NoteOff))
import System.MIDI.Utility (selectOutputDevice)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Note

type Seq = [[Int]]
type ChanSeq = (Int, Seq)

data Command = Notes [Maybe Note]
             | Mute
             | Rev
             | Ln Int
             | Ch Int
             | Mv Seq
             | Tr Seq
             | Reverb Seq deriving Show

data Token = Hash
           | Dash
           | Dot
           | Symbol String
           | Float Float
           | Int Int deriving Show

bpm :: Int
bpm = 120

-- PARSE

consumeNotes :: [Token] -> ([Command], [Token])
consumeNotes [] = ([],[])
consumeNotes xs = mapFst ((:[]) . Notes . map transform) $ span pred xs where

  pred (Symbol x) = case stringToNote x of
                    Nothing -> False
                    Just x  -> True
  pred Dot        = True
  pred _          = False

  transform (Symbol x) = stringToNote x
  transform _          = Nothing

consumeCommands :: [Token] -> ([Command], [Token])
consumeCommands [] = ([],[])
consumeCommands xs = result where
  (command,xs') = consumeCommand xs
  result        = case command of
    Nothing      -> ([],xs)
    Just command -> mapFst (command :) $ consumeCommands xs'

consumeCommand :: [Token] -> (Maybe Command, [Token])
consumeCommand []                   = (Nothing,[])
consumeCommand (Dash:(Symbol s):xs) = match xs s where
  match :: [Token] -> String -> (Maybe Command, [Token])
  match xs "mute"   = (Just Mute, xs)
  match xs "rev"    = (Just Rev, xs)
  match xs "ln"     = mapFst (Just . Ln) $ consumeInt xs
  match xs "ch"     = mapFst (Just . Ch) $ consumeInt xs
  match xs "mv"     = mapFst (Just . Mv) $ consumeSeq xs
  match xs "tr"     = mapFst (Just . Tr) $ consumeSeq xs
  match xs "reverb" = mapFst (Just . Reverb) $ consumeSeq xs
  match xs _        = (Nothing, xs)

  consumeSeq :: [Token] -> (Seq, [Token])
  consumeSeq []             = ([],[])
  consumeSeq ((Int i):xs)   = mapFst ([i] :) $ consumeSeq xs
  consumeSeq ((Float f):xs) = mapFst ([truncate (f * 255)] :) $ consumeSeq xs
  consumeSeq xs             = ([],xs)

  consumeInt :: [Token] -> (Int, [Token])
  consumeInt []             = (0,[])
  consumeInt ((Int i):xs)   = (i,xs)
  consumeInt ((Float f):xs) = (truncate (f * 255), xs)
  consumeInt xs             = (0,xs)
consumeCommand (_:xs)               = (Nothing,xs)

parse :: String -> [Command]
parse = parse' . map match . words where
  parse' :: [Token] -> [Command]
  parse' []        = []
  parse' (Hash:xs) = notes ++ commands ++ parse' xs'' where
    (notes, xs')     = consumeNotes xs
    (commands, xs'') = consumeCommands xs'
  parse' _         = []

  match :: String -> Token
  match "#" = Hash
  match "-" = Dash
  match "." = Dot
  match s   = maybe' Int (readMaybe s :: Maybe Int)
            $ maybe' Float (readMaybe s :: Maybe Float)
            $ Symbol s

-- EVAL

eval :: [Command] -> [ChanSeq]
eval []              = []
eval ((Notes ns):xs) = seq : eval xs' where (seq, xs') = evalNotes ns xs
eval _               = []

evalNotes :: [Maybe Note] -> [Command] -> (ChanSeq, [Command])
evalNotes ns xs = evalCommands xs (1, map transformNote ns) where
  transformNote :: Maybe Note -> [Int]
  transformNote Nothing  = []
  transformNote (Just n) = [ noteToInt n ]

  evalCommands :: [Command] -> ChanSeq -> (ChanSeq, [Command])
  evalCommands (Mute:xs) seq       = evalCommands xs $ mute seq
  evalCommands (Rev:xs) seq        = evalCommands xs $ rev seq
  evalCommands ((Ln l):xs) seq     = evalCommands xs $ ln l seq
  evalCommands ((Ch c):xs) seq     = evalCommands xs $ ch c seq
  evalCommands ((Mv s):xs) seq     = evalCommands xs $ mv s seq
  evalCommands ((Tr s):xs) seq     = evalCommands xs $ tr s seq
  -- evalCommands ((Reverb s):xs) seq = evalCommands xs $ reverb s seq
  evalCommands ((Reverb s):xs) seq = evalCommands xs seq
  evalCommands xs seq              = (seq, xs)

mute :: ChanSeq -> ChanSeq
mute (c,seq) = (c,map (\_ -> []) seq)

rev :: ChanSeq -> ChanSeq
rev (c,seq) = (c, reverse seq)

ln :: Int -> ChanSeq -> ChanSeq
ln l (c,seq) = (c, concat $ map (\x -> x : (take (l - 1) $ repeat [])) seq)

ch :: Int -> ChanSeq -> ChanSeq
ch c (_,seq) = (c,seq)

mv :: Seq -> ChanSeq -> ChanSeq
mv s (c,seq) = (c,seq) -- TODO

tr :: Seq -> ChanSeq -> ChanSeq
tr s (c,seq) = (c, map (\(ns,x) -> map (+ x) ns) $ zip seq $ cycle $ concat s)

-- reverb :: Seq -> ChanSeq -> ChanSeq -- TODO this needs events instead of note values

-- HELPERS

maybe' :: (a -> b) -> Maybe a -> b -> b
maybe' f m d = maybe d f m

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a, b)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (a,b) = (a, f b)

-- SEQUENCE

sequencer :: Connection -> Int -> String -> IO ()
sequencer output n file = do
  input <- readFile file
  -- seqs  <- return $ parse input
  -- seqs' <- return $ zip [1..] seqs
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

-- MAIN

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
