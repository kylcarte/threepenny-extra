{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Threepenny.Extra where

import Reactive.Threepenny
import Graphics.UI.Threepenny.Extra
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

type ToggleT    = T Bool
type RangeT     = T Percent
type SelectT a  = T (Maybe a)
type ChoicesT a = T [a]

-- Percent {{{

data Percent = Percent
  { pNumerator   :: Rational
  , pDenomenator :: Rational
  } deriving (Eq,Ord)

instance Show Percent where
  showsPrec _ (Percent n d) = showsDecimal 2 (fromRational $ n / d)
    . showChar '%'

_0 :: Percent
_0 = Percent 0 1

showsDecimal :: Int -> Double -> ShowS
showsDecimal n = showString
  . (\(b,a) -> if length a `elem` [0,1] then b else b ++ take (n + 1) a)
  . L.break (== '.')
  . L.dropWhileEnd (== '0')
  . show

percRange :: (Ord a, Num a) => a -> a
percRange n = if
  | n < 0 -> 0
  | n > 1 -> 1
  | True  -> n

(%%%) :: (Rational -> Rational -> Rational) -> (Rational -> Rational -> Rational) -> Percent -> Percent -> Percent
(f %%% g) (Percent n1 d1) (Percent n2 d2) = Percent (f n1 n2) (g d1 d2)
infixr 3 %%%

instance Num Percent where
  fromInteger i = Percent n $ if n == 0 then 0 else 1
    where
    n = percRange $ fromInteger i
  (+) = (+) %%% (+)
  negate (Percent n d) = Percent (d - n) d
  (*) = (*) %%% (*)
  abs    = error "Percent: abs undefined"
  signum = error "Percent: signum undefined"

instance Fractional Percent where
  fromRational = (`Percent` 1) . percRange
  recip (Percent n d) = Percent d n

instance Real Percent where
  toRational (Percent n d) = n / d

perc :: Percent -> Percent
perc = id

-- }}}

toggleT :: Bool -> Event a -> UI ToggleT
toggleT def ev = updateT ev <$> accumB def (not <$ ev)

rangeT :: (Fractional a,Real a) => Percent -> a -> a -> Event a -> UI RangeT
rangeT def lo hi = stepperT def
  . fmap (fromRational . toRational . mkPerc (hi - lo))
  where
  mkPerc d = if
    | d == 0 -> const 0
    | True   -> (/ d) . bracket d . ((-) <&> lo)
  bracket h a = if
    | a < 0 -> 0
    | a > h -> h
    | True  -> a

mapT :: Ord k => B (Map k a) -> Event (Maybe k) -> UI (SelectT a)
mapT = selectT .% lookupE

lookupE :: Ord k => B (Map k a) -> Event (Maybe k) -> Event (Maybe a)
lookupE b e = go <$> e <!> b
  where
  go k m = k >>= M.lookup <&> m

selectT :: Event (Maybe a) -> UI (SelectT a)
selectT = stepperT Nothing

stepperT :: a -> Event a -> UI (T a)
stepperT a e = tidings <$> stepper a e <&> e

updateT :: Event b -> Behavior a -> Tidings a
updateT e b = tidings b (b <@ e)

accumT :: a -> Event (a -> a) -> UI (T a)
accumT a e = updateT e <$> accumB a e

listB :: [B (Maybe a)] -> B [a]
listB = fmap catMaybes . sequenceA

(<!>) :: Event (a -> b) -> Behavior a -> Event b
e <!> b = ((&) <$> b) <@> e
infixl 4 <!>

