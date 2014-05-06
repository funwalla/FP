module Main where

import qualified Hat.PreludeBasic 
import qualified Prelude 
import Hat.Hack 
import qualified Hat.Hat as T 
import Hat.Hat  (WrapVal(wrapVal))
import Hat.Prelude 

gfilter' ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.Fun a Bool) (T.Fun (T.List a) (T.List a)))

hfilter' ::
  (T.R (T.Fun a Bool)) -> (T.R (T.List a)) -> T.RefExp -> T.R (T.List a)

gfilter' pfilter' p = T.fun2 afilter' pfilter' p hfilter'

hfilter' fp (T.R T.List _) p = T.con0 p2v20v2v21 p T.List T.aList
hfilter' fp (T.R (T.Cons fx fxs) _) p =
  T.cguard p3v22v3v24 p (T.ap1 p3v22v3v24 p fp fx)
    (\ p ->
      T.con2 p3v34v3v49 p T.Cons T.aCons fx
        (T.app2 p3v38v3v49 p3v38v3v44 p afilter' hfilter' fp fxs))
    (\ p ->
      T.cguard p4v22v4v30 p (gotherwise p4v22v4v30 p)
        (\ p -> T.app2 p4v34v4v45 p4v34v4v40 p afilter' hfilter' fp fxs)
        (\ p -> T.fatal p))
hfilter' _ _ p = T.fatal p

tMain = T.mkModule "Main" "filt.hs" Prelude.True

afilter' = T.mkVariable tMain 20001 40045 3 2 "filter'" Prelude.False

p2v1v4v45 = T.mkSrcPos tMain 20001 40045

p2v20v2v21 = T.mkSrcPos tMain 20020 20021

p3v22v3v24 = T.mkSrcPos tMain 30022 30024

p3v34v3v49 = T.mkSrcPos tMain 30034 30049

p3v38v3v49 = T.mkSrcPos tMain 30038 30049

p3v38v3v44 = T.mkSrcPos tMain 30038 30044

p4v22v4v30 = T.mkSrcPos tMain 40022 40030

p4v34v4v45 = T.mkSrcPos tMain 40034 40045

p4v34v4v40 = T.mkSrcPos tMain 40034 40040

main = T.traceIO "filt" (Main.gmain T.mkNoSrcPos T.mkRoot)
