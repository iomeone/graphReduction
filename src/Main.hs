module Main where


import Text.Parsec


import Parser


main :: IO ()
main = 
  case parse pProgram "core" "fac n = if (n==0) 1 (n * fac (n-1)) ; main = fac 4" of
    Left  _       ->  do
                        putStrLn "wrong"
    Right program ->  do
                        putStrLn $ show program
  
  >>

  putStrLn "hello world"
