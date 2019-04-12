module WatchFile where
import           Control.Monad      (forever)
import           Control.Concurrent (threadDelay)

import           Text.Parsec

import           System.Directory  
import           System.FilePath      
import           System.FSNotify    (Event (..), watchTree, withManager, watchDir)
                             -- </>
 


import AstDraw
import Compile
import Parser
import Types
import UTF8


toCompile :: Program ->  IO ()
toCompile program = do
    putStrLn "-------------------------\n Init state is:" 
    putStrLn $ show $compile $ program
    





type Action = FilePath -> IO ()

callback :: Action -> Event -> IO ()
callback action (Added filepath _ _)    = action  filepath 
callback action (Modified filepath time b) = action filepath 
callback action (Removed filepath _ _)  = action filepath
callback action (Unknown  filepath _ _)  = action filepath 



watchDirectoryTree :: FilePath -> Action -> IO ()
watchDirectoryTree filepath action =
  withManager  $ \mgr -> do
    print $ "Watching " ++ filepath
    watchDir mgr filepath (const True) $ callback action         -- (callback action)
    forever $ threadDelay 1000000



compileFileWhenModified :: String -> IO ()
compileFileWhenModified path = do
  if takeExtension path == ".txt"
  then do
    putStrLn "-------------------------"
    putStrLn $ "compiling " ++ path ++ "... "
    src <- readUTF8File  (takeFileName path) 
    putStrLn "source code: " >> putStrLn src

    p <- return $ parse pProgram "core" src
    case p of 
      Left _ -> putStrLn "wrong AST"
      Right program -> do
        putStrLn "-------------------------\n AST is:"
        putStrLn $ show program
        curDir <- getCurrentDirectory
        drawAstEx ["raw"] (curDir </> "png" </> "ast") [ program ]
        toCompile program
         
                



    putStrLn "-------------------------"
  else
    putStrLn $ path ++ " is changing." 


    

startTask :: IO ()
startTask = do
    curDir <- getCurrentDirectory
    src <- readUTF8File (curDir </> "htp.txt")
    p <- return $ parse pProgram ""  src
    case p of 
      Left _ -> putStrLn "wrong"
      Right program -> do
        putStrLn "-------------------------\n AST is:"
        putStrLn $ show program
        toCompile program
  
    putStrLn "-------------------------"
    watchDirectoryTree curDir compileFileWhenModified
    forever $ threadDelay 5000000    