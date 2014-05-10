module Main where

-- main = do s <- getLine
--           writeFile "testFile.txt" s
--           putStrLn (s ++ " written to file.")

main = putStr' "hello, bob\n"

putCharList    :: String -> [IO ()]
putCharList [] =  []
putCharList (c:cs) = putChar c: putCharList cs

putStr'   :: String -> IO ()
putStr' s =  sequence_ (putCharList s)
