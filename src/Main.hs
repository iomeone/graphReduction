module Main where

-- hackage
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)

import           System.FilePath                                   -- </>
import           System.Directory   

import           Text.Parsec


-- self defined
import AstDraw
import Compile
import Parser
import State
import Types
import UTF8
import WatchFile






compileFileWhenModified :: String -> IO ()
compileFileWhenModified path = do
  if takeExtension path == ".txt"
  then do
    putStrLn "-------------------------"
    putStrLn $ "compiling " ++ path ++ "... "
    src <- readUTF8File  (takeFileName path) 
    putStrLn "source code: "
    putStrLn src
    p <- return $ parse pProgram "core" src
    case p of 
      Left _ -> putStrLn "wrong"
      Right program -> do
        putStrLn $ show program
        curDir <- getCurrentDirectory
        drawAstEx ["raw"] (curDir </> "png" </> "ast") [ program ]

    putStrLn "-------------------------"
  else
    putStrLn $ path ++ " is changing." 
    




main :: IO ()
main = do
  curDir <- getCurrentDirectory
  src <- readUTF8File (curDir </> "htp.txt")
  p <- return $ parse pProgram ""  src
  case p of 
    Left _ -> putStrLn "wrong"
    Right program -> do
      putStrLn $ show program


  watchDirectoryTree curDir compileFileWhenModified
  forever $ threadDelay 5000000
