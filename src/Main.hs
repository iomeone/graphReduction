module Main where

-- hackage
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)

import           System.FilePath                                   -- </>
import           System.FSNotify    (Event (..), watchTree, withManager, watchDir)
import           System.Directory   

import           Text.Parsec


-- self defined
import AstDraw
import Compile
import Parser
import State
import Types
import UTF8




type Action = FilePath -> IO ()

callback :: Action -> Event -> IO ()
callback action (Added filepath _ _)    = action  filepath 
callback action (Modified filepath time b) = action filepath 
callback action (Removed filepath _ _)  = action filepath
callback action (Unknown  filepath _ _)  = action filepath 



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
    



watchDirectoryTree :: FilePath -> Action -> IO ()
watchDirectoryTree filepath action =
  withManager  $ \mgr -> do
    print $ "Watching " ++ filepath
    watchDir mgr filepath (const True) $ callback compileFileWhenModified         -- (callback action)
    forever $ threadDelay 1000000

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
