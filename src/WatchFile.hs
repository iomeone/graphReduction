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
import Eval
import Parser
import StackDraw
import State
import Types
import UTF8


-- draw_Addr_Node_List :: [(Addr, Node)] -> 





addrToNode :: ([Addr], [(Addr, Node)]) -> [(Addr, Node)]
addrToNode stackAddrList_AddrNodeList@(stackAddrList, addrNodeList)= 
    fmap addrToAddrNode stackAddrList 
    where 
        addrToAddrNode :: Addr -> (Addr, Node)
        addrToAddrNode addr = 
            (addr, (aLookup addrNodeList addr (error "addrToAddrNode")))



showStackProcess :: [([Addr], [(Addr, Node)])] -> IO ()
showStackProcess stackAddrList_AddrNodeList_List = do
    putStrLn $ unlines x
    return  ()
    where 
        x =  fmap show (  fmap addrToNode stackAddrList_AddrNodeList_List)







getStack :: TiState -> ([Addr], [(Addr, Node)])
getStack state@(addrLst, dump, heap@(Heap size freeList addrNodeList), globals, steps) = 
    (addrLst, addrNodeList)



toCompile :: Program ->  IO ()
toCompile program = do
    putStrLn "-------------------------\n Init state is:" 
    putStrLn $ show state
    putStrLn "-------------------------\n Evaled states is:" 
    showStackProcess stackAddrList_AddrNodeList_List

    curDir <- getCurrentDirectory
    drawStackEx ["eval"] (curDir </> "png" </> "eval") [  reverstack stackAddrList_AddrNodeList_List ]

    where
        state = compile program
        states = eval state
        stackAddrList_AddrNodeList_List = fmap getStack states
        reverstack :: [([Addr], [(Addr, Node)])] -> [([Addr], [(Addr, Node)])] 
        reverstack ((addrlist, assocList) : xs) = 
            (reverse addrlist, assocList) : (reverstack xs)
        reverstack [] = []





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