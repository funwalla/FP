

putStrLn' :: String -> IO ()
putStrLn' =  putStr . (++ "\n")

print :: Show a => a -> IO ()
print = putStrLn' . show

helloWorld :: IO ()
helloWorld =  putStrLn' "Hello, World!"
