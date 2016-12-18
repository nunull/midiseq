module Parse where

import Text.Read (readMaybe)
import Helpers
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
