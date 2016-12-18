module Eval where

import Note
import Parse

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
