{-# LANGUAGE DataKinds #-}

module Main (main) where

import qualified Prelude as P 
import Lib
import Language.Copilot hiding (alwaysBeen, since)
import Copilot.Language.Spec
import Copilot.Language.Stream
import Copilot.Compile.C99
import Lib (oneShotRise, alwaysBeen', debounceRise, debounce, srsFF, once', weakPrevious, atLast)
import qualified Data.Bifunctor as BF

lim :: (Typed a, Ord a) => Stream a -> Stream a -> Stream a -> Stream a
lim l u x = mux (x > u) u $ mux (x < l) l x

progCounter :: Stream Bool -> Stream Bool -> Stream Int16
progCounter plus minus = lim 0 9999 cnt
  where 
    cnt = mux plus (z + 1) $ mux minus (z - 1) z :: Stream Int16
    z = [0] ++ cnt


digit :: Stream Word8
digit = [0, 1] ++ digit

digitPow :: Stream Word8 -> Stream Word16
digitPow x = ar ! cast x
  where 
    ar :: Stream (Array 4 Word16)
    ar = Const $ array [1, 10, 100, 1000]

buttonPlus, buttonMinus, buttonA, buttonB :: Stream Bool
buttonPlus = extern "button_plus" Nothing
buttonMinus = extern "button_minus" Nothing
buttonA = debounce 40 $ extern "button_A" Nothing
buttonB = debounce 40 $ extern "button_B" Nothing

data MyState = StayLit | BlinkSlow | BlinkFast | ResetTimer deriving (Enum)

caseAction :: Stream Word8 -> [(Stream Word8, Stream Bool)] -> Stream Bool
caseAction cAse l = f l false
  where
    f :: [(Stream Word8, Stream Bool)] -> (Stream Bool -> Stream Bool)
    f = foldl (\acc (s, a) -> (\x -> acc $ mux (cAse == s) a x)) id

stateToBlink :: Stream Word8 -> Stream Bool
stateToBlink nowState = caseAction nowState $ bmmap (Const . myStateToW8) id  
                              [
                                (StayLit, true)
                              , (BlinkSlow, blink 500)
                              , (BlinkFast, blink 100)
                              , (ResetTimer, false)
                              ]
  where
    blink len = toggle $ clk (period len) (phase 0)
    bmmap f1 f2 = map (BF.bimap f1 f2)

myStateToW8 :: MyState -> Word8
myStateToW8 x = toEnum (P.fromEnum x)

myStateFlow ::Stream Bool -> Stream Bool -> Stream Bool
            -> [(Stream Word8, [(Stream Bool, Stream Word8)])]
myStateFlow a b t = bmmap cvt (bmmap id cvt)
                $ [
                    (StayLit,  [ (a, BlinkSlow)
                                , (b, StayLit)])
                  , (BlinkSlow, [ (a, StayLit)
                                , (b, BlinkFast)])
                  , (BlinkFast, [ (a, BlinkSlow)
                                , (b, ResetTimer)
                                , (t, StayLit) ])
                  , (ResetTimer, [(true, BlinkFast)])
                  ]
  where
    cvt = Const . myStateToW8
    bmmap f1 f2 = map (BF.bimap f1 f2)

myState :: Stream Bool -> Stream Bool -> Stream Word8
myState a b = state
  where
    state = [myStateToW8 StayLit] ++ nextState
    nextState = stateFlow (myStateFlow a b blinkTimer) state
    blinkTimer :: Stream Bool
    blinkTimer = timer (Const 2000) (oneShotRise (state == Const (myStateToW8 BlinkFast)))


progs :: [Arg]
progs = [arg $ P.foldl1 (||)
        $ map (\(n, f) -> cnt == n && f (cnt == n && buttonA) (cnt == n && buttonB)) l]
  where l = [
              (0, \a b -> once' 100 $ oneShotRise a)
            , (1, \a b -> once' 100 $ oneShotFall a)
            , (2, \a b -> toggle $ tr a)
            , (3, \a b -> srsFF a b)
            , (4, \a b -> alwaysBeen' 1000 a)
            , (5, \a b -> once' 1000 a)
            , (6, \a b -> weakPrevious a)
            , (7, \a b -> strongPrevious a)
            , (8, \a b -> since a b)
            , (9, \a b -> since' a b)
            , (10, \a b -> backTo a b)
            , (11, \a b -> atLast' a b)
            , (12, \a b -> after' a b)
            , (13, \a b -> debounce 2000 a)
            , (14, \a b -> since' a $ once' 1000 b)
            , (15, \a b -> seriesPattern
                            -- (wanted signal, signal for cancelation)
                            [ (tr b, tr a)   
                            , (tr b, tr a)
                            , (tr a, tr b)
                            , (tr a, tr b)])
            , (16, \a b -> stateToBlink $ myState (tr a) (tr b))
            ] :: [(Stream Word16, Stream Bool -> Stream Bool -> Stream Bool)]
        tr = oneShotRise


cnt = unsafeCast $ progCounter trigPlus trigMinus :: Stream Word16
  where
    trigPlus = debounceRise 20 buttonPlus
    trigMinus = debounceRise 20 buttonMinus

spec :: Spec
spec = do
  let 
    digitNum = unsafeCast
      $ (cnt `div` digitPow digit) `mod` 10 :: Stream Word8
  trigger "show_7seg" true [arg digit, arg digitNum]
  trigger "change_led" true progs

  return ()


main :: IO ()
main = do
  -- interpret 20 spec
  reify spec >>= compile "copilot_cords"
