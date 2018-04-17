module Main where

import Lib
import System.TimeIt
import System.IO
import System.Environment
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

main = do
  args <- getArgs
  case args of
    ["precompute"] -> do
      grid <- getGrid "regular"
      saveAll grid
    [mode, inputFile] -> do
      grid <- getGrid mode
      tests <- TextIO.readFile inputFile
      runTests grid tests
    [mode] -> do
      grid <- getGrid mode
      mainLoop grid
    _ -> do
      putStrLn "invalid program args, try:"
      putStrLn "codejam precompute     -- To build the connections file"
      putStrLn "codejam fast           -- to use the connections file and not the dictionary file"
      putStrLn "codejam regular        -- to only use the dictionary file"
      putStrLn "codejam fast input.txt -- to run all of the tests in the input file"


runTests grid tests = do
  mapM_ (timeIt . runTest grid) $ Text.lines tests

runTest grid line = putStrLn $ show $ search grid start end
  where (start:end:_) = Text.words line

mainLoop dic = do
  putStr "Start Word:"
  hFlush stdout
  start <- TextIO.getLine
  putStr "End Word:"
  hFlush stdout
  end <- TextIO.getLine
  putStrLn $ show $ search dic start end

  putStr "Go Again? (Y/n)"
  hFlush stdout
  goAgain <- getLine
  if goAgain /= "n" then mainLoop dic else return ()

saveAll grid = do
  TextIO.writeFile "connections.txt" $ Text.unlines save
  where
    save = saveGrid grid

getGrid pre =
  case pre of
    "fast" -> precomputed
    _ -> lazycomputed
  where
    precomputed = do
      content <- readFile "connections.txt"
      return (loadGrid $ Text.lines $ Text.pack content)
    lazycomputed = do
      content <- readFile "dictionary.txt"
      return (buildWordGrid $ Text.lines $ Text.pack content)

portableLines [] = []
portableLines text =
  let (pre, suf) = break isLineTerminartor text
  in pre : case suf of
    ('\r':'\n':rest) -> portableLines rest
    ('\r':rest)      -> portableLines rest
    ('\n':rest)      -> portableLines rest

isLineTerminartor c = c == '\r' || c == '\n'
