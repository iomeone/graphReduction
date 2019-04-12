module WatchFile where
import           System.FilePath                                   -- </>
import           System.FSNotify    (Event (..), watchTree, withManager, watchDir)
import           Control.Monad      (forever)
import           Control.Concurrent (threadDelay)





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





    