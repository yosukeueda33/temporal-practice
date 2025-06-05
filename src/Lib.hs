module Lib where

import qualified Prelude as P 
import Language.Copilot hiding (alwaysBeen, since)
import Copilot.Language.Spec
import Copilot.Compile.C99


oneShotRise, oneShotRise'  :: Stream Bool -> Stream Bool
oneShotRise s = s && not (sP s)
oneShotRise' s = s && not prev
  where
    prev = [False] ++ s

oneShotFall, oneShotFall' :: Stream Bool -> Stream Bool
oneShotFall s = not s && sP s
oneShotFall' s = not s && prev
  where
    prev = [False] ++ s

toggle :: Stream Bool -> Stream Bool
toggle s = r
  where
    r = [False] ++ mux s (not r) r

srsFF :: Stream Bool -> Stream Bool -> Stream Bool
srsFF setSig resetSig = r
  where
    r = mux setSig true $ mux resetSig false prev
    prev = [False] ++ r

alwaysBeen' :: Stream Int32 -> Stream Bool -> Stream Bool
alwaysBeen' len s = cnt >= (len - 1) -- && s
  where
    cnt = [0] ++ mux s (cnt + 1) 0


once' :: Stream Int32 -> Stream Bool -> Stream Bool
once' len s = cnt > 0
  where
    cnt = [0] ++ mux s len (lim 0 $ cnt - 1)
    lim l x = mux (l < x) x l

timer :: Stream Int32 -> Stream Bool -> Stream Bool 
timer len s = oneShotFall $ once' len s

weakPrevious, wP :: Stream Bool -> Stream Bool
weakPrevious s = [True] ++ s
wP = weakPrevious

strongPrevious, sP :: Stream Bool -> Stream Bool
strongPrevious s = [False] ++ s
sP = strongPrevious

since, since' :: Stream Bool -> Stream Bool -> Stream Bool
since a b = v
  where
    v = [False] ++ b || (a && v)

since' a b = a && since a b

backTo :: Stream Bool -> Stream Bool -> Stream Bool
backTo a b = v
  where
    v = [True] ++ b || (a && v)

atLast, atLast' :: Stream Bool -> Stream Bool -> Stream Bool
atLast a b = v
  where
    v = [True] ++ (b ==> a) && (not b ==> v)

atLast' a b = v
  where
    v = [False] ++ (b ==> a) && (not b ==> v)

after, after' :: Stream Bool -> Stream Bool -> Stream Bool
after a b = v
  where
    v = [True] ++ (not b) && (a || v)

after' a b = v
  where
    v = [False] ++ (not b) && (a || v)

debounce :: Stream Int32 -> Stream Bool -> Stream Bool 
debounce len s = srsFF trigRise trigFall
  where
    trigRise = oneShotRise $ alwaysBeen' len s
    trigFall = oneShotRise $ alwaysBeen' len $ not s

debounceRise len = oneShotRise . alwaysBeen' len

seriesPattern :: [(Stream Bool, Stream Bool)] -> Stream Bool
seriesPattern = foldl f true
  where f acc (s, cancel) = srsFF (sP acc && s) cancel

stateFlow :: [(Stream Word8, [(Stream Bool, Stream Word8)])]
          -> Stream Word8 -> Stream Word8
stateFlow flow nowState =
    foldl (\acc (s, xs) -> mux  (s == nowState) (f xs) acc) nowState flow 
  where
    f :: [(Stream Bool, Stream Word8)] -> Stream Word8
    f = foldl (\acc (signal, s) -> mux signal s acc) nowState