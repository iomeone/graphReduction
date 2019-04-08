module Main where

-- hackage
import           System.FilePath                                   -- </>
import           System.Directory   

import Text.Parsec


-- self defined
import AstDraw
import Parser
import Types




main :: IO ()
main = 
  -- putStrLn $ show $ ELam ["haha"] (ENum 4)

  -- fac n = if (n==0) 1 (n * fac (n-1)) ; main = fac 4;


  case parse pProgram "core" "fac n = if (n==0) 1 (n * fac (n-1)) ; main = fac 4;" of
    Left  _       ->  do
                        putStrLn "wrong"
    Right program ->  do
                        putStrLn $ show program
                        curDir <- getCurrentDirectory
                        drawAstEx ["raw"] (curDir </> "png" </> "ast") [ program ]
  
  -- >>

  -- putStrLn "hello world"

