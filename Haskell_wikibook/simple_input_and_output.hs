

-- main = do
--   putStrLn "The base?"
--   base <- getLine
--   putStrLn "The height?"
--   height <- getLine
--   putStrLn ("Area: "++ show (0.5 * read base * read height))

-- main = do
--   putStrLn "Please enter your name: "
--   getLine
--   putStrLn ("Hello, how are you?")

doGuessing num = do
   putStrLn "Enter your guess:"
   guess <- getLine
   if (read guess) < num
     then do putStrLn "Too low!"
             doGuessing num
     else
       if (read guess) > num
         then do putStrLn "Too high!"
                 doGuessing num
         else    putStrLn "You Win!"

nameRequest = do
  putStrLn "What is your name?"
  name <- getLine
  if name == "Simon" || name == "John" || name == "Phil"
     then putStrLn "Great language!"
     else 
       if name == "Koen"
          then putStrLn "Debugging is fun."
          else putStrLn "Who?"
