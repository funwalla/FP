main = do
  a <- return "Hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b
  
--   line <- getLine
--   if null line
--      then return ()
--      else do
--           putStrLn $ reverseWords line
--           main

-- reverseWords :: String -> String
-- reverseWords =  unwords . map reverse . words
    
