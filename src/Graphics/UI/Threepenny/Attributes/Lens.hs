{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Graphics.UI.Threepenny.Attributes.Lens where

import Graphics.UI.Threepenny.Core hiding
  (text,children,set,attr)
import Control.Lens hiding (set',view,(^.))
import qualified Control.Lens as Lens
import Data.Aeson (FromJSON(..),Value)
import qualified Data.Aeson as JSON
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)

type LensUI    s t a b = Lens (UI s) (UI s) (UI a) b
type LensUI'   s   a   = LensUI s s a a
type GetterUI  s   a   = Getter (UI s) (UI a)
type SetterUI  s t a   = Setter (UI s) (UI t) () a
type SetterUI' s   a   = SetterUI s s a

type PropLens   s a   = LensUI s s (Maybe a) a
type PropGetter s a   = GetterUI s (Maybe a)

fromAttr :: ReadWriteAttr x i o -> LensUI x x o i
fromAttr a = lens g s
  where
  g mx   = mx >>= get' a
  s mx i = mx >>= (<$) <$> id <*> set' a i

jsLens :: (x -> JSFunction o) -> (x -> i -> JSFunction ()) -> LensUI x x o i
jsLens gf sf = lens g s
  where
  g mx   = do
    x <- mx
    callFunction $ gf x
  s mx i = do
    x <- mx
    runFunction $ sf x i
    return x

jsGetter :: FromJSON o => (x -> JSFunction Value) -> PropGetter x o
jsGetter g = to $ \mx -> do
  x <- mx
  v <- callFunction $ g x
  return $ parseMaybe parseJSON v

jsSetter :: (x -> i -> JSFunction y) -> SetterUI x y i
jsSetter s = setting $ \i mx -> do
  x <- mx
  callFunction $ s x $ i ()

jsSetter' :: (x -> i -> JSFunction ()) -> SetterUI' x i
jsSetter' s = setting $ \i mx -> do
  x <- mx
  runFunction $ s x $ i ()
  return x

view :: LensLike (Const a) s t a b -> s -> a
view = Lens.view . coerced
{-# INLINE view #-}

(^.) :: s -> LensLike (Const a) s t a b -> a
(^.) = flip view
{-# INLINE (^.) #-}

text :: LensUI' Element String
text = jsLens
  (ffi "$(%1).text()")
  (ffi "$(%1).text(%2)")

{-
prop :: String -> PropLens Element String
prop s = 
-}

hasProp :: String -> GetterUI Element Bool
hasProp s = jsGetter (ffi "$(%2).is(%1)" $ "[" ++ s ++ "]")
  . to (fromMaybe False <$>)

jsonStr :: FromJSON a => String -> Maybe a
jsonStr = JSON.decode . BS.pack

