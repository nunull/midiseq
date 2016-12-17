module Main where

import Note (Note (C, Ds, G, Gs), noteToInt)
import Control.Concurrent (forkIO, killThread, threadDelay)
import System.MIDI (Connection, openDestination, start, stop, send, close, currentTime)
import System.MIDI.Base (MidiMessage (MidiMessage), MidiMessage' (NoteOn, NoteOff))
import System.MIDI.Utility (selectOutputDevice)

-- import System.Eval.Haskell (eval)
import GHC
import GHC.Paths (docdir, libdir, ghc, ghc_pkg)
import GhcMonad (liftIO)
import Data.Dynamic (fromDyn)
import Name (getOccString)

sequencer :: Connection -> Maybe Note -> [Note] -> IO ()
sequencer output prev notes = do
  t <- currentTime output
  putStrLn $ "t: " ++ show t

  case prev of
    Nothing   -> return ()
    Just note -> send output $ MidiMessage 1 $ NoteOff (noteToInt note) 255

  case notes of
    []     -> return ()
    (x:xs) -> do
      send output $ MidiMessage 1 $ NoteOn (noteToInt x) 255
      threadDelay 1000000
      sequencer output (Just x) xs

notes = Just [ C, Ds, G, Gs ]

main :: IO ()
main = do
  putStrLn libdir
  putStrLn docdir
  putStrLn ghc
  putStrLn ghc_pkg

  runGhc (Just libdir) $ do
    -- modSums <- initSession ["Main", "Note"]
    -- let thisModSum = head modSums
    -- exports <- listExports thisModSum
    -- mapM_ (liftIO . putStrLn) exports

    -- importDecl_RdrName <- parseImportDecl "import Note"
    -- setContext [IIDecl importDecl_RdrName]
    dynVal <- dynCompileExpr "1 + 2" -- "[ C, Ds, G, Gs ]"
    liftIO $ print $ (fromDyn dynVal "nope-nothing")

  outputDevice <- selectOutputDevice "output device:" $ Just "SimpleSynth virtual input"
  putStrLn "press enter to exit." 

  -- notes <- eval "[ C, Ds, G, Gs ]" ["Note"] :: IO (Mayb [Note])
  case notes of
    Nothing    -> putStrLn "error."
    Just notes -> do
      output <- openDestination outputDevice
      start output

      thread <- forkIO (sequencer output Nothing notes)
      getLine

      killThread thread
      close output

initSession modStrNames = do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags {
    hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
    }
  targets <- mapM
              (\modStrName -> do
                  liftIO $ putStrLn modStrName
                  target <- guessTarget ("*"++modStrName++".hs") Nothing
                  return target
              ) modStrNames
  setTargets targets
  load LoadAllTargets
  modSums <- mapM
              (\modStrName -> do
                  liftIO $ putStrLn modStrName
                  modSum <- getModSummary $ mkModuleName modStrName
                  return $ ms_mod modSum
              ) modStrNames
  return modSums

listExports mod = do
  maybeModInfo <- getModuleInfo mod
  case maybeModInfo of
    (Just modInfo) -> do
      let expNames = modInfoExports modInfo
          expStrNames = map getOccString expNames
      return expStrNames
