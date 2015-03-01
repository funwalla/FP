module Ch_3_simple_graphics_1 where

import SOE

main0 :: IO()
main0 = runGraphics (
                      do w <- openWindow "1st Graphics Program" (300, 300)
                         drawInWindow w (text (100, 200) "Hello Graphics World.")
                         spaceClose w
                    )

spaceClose   :: Window -> IO ()
spaceClose w =  do k <- getKeyEx w True
                   if k == ' ' then closeWindow w
                               else spaceClose w

putCharList          :: String -> [IO ()]
putCharList []       =  []
putCharList (c : cs) =  putChar c : putCharList cs

actionList :: [IO ()]
actionList =  putCharList "This is a test"

doList :: IO ()
doList =  sequence_ actionList
