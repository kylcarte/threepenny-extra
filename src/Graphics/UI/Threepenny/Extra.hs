{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.UI.Threepenny.Extra
  ( module Graphics.UI.Threepenny.Extra
  , module Exports
  ) where

import Graphics.UI.Threepenny.Core             as Exports hiding
  (attr,text,children,(<**>),Const(..),style,newEvent,(#.))
import Graphics.UI.Threepenny.Attributes.Extra
import qualified Foreign.JavaScript.Marshal    as JS
import qualified Graphics.UI.Threepenny.Core   as Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Data.List                     as L
import qualified Data.ByteString.Lazy.Char8    as BS
import Data.Aeson (ToJSON(..),FromJSON(..),encode,decode,Value)
import Data.Aeson.Types (parseMaybe)
import Control.Monad                           as Exports
  (void,(>=>))
import Graphics.UI.Threepenny.Action

-- UI Functionality {{{

type E     = Event
type B     = Behavior
type H m a = a -> m ()
type T     = Tidings

newEvent :: (MonadIO f, MonadIO g) => f (E a,H g a)
newEvent = do
  (e,h) <- liftIO Core.newEvent
  return (e,liftIO . h)

runLater :: UI void -> UI ()
runLater m = do
  w <- window
  liftIOLater $ void $ runUI w m

window :: UI Window
window = askWindow

windows :: (Window -> UI a) -> UI a
windows = (window >>=)

addCSSFile :: FilePath -> UI ()
addCSSFile = windows . flip UI.addStyleSheet

newRefE :: a -> UI (Ref a,E a,H UI a)
newRefE a = do
  r <- newRef a
  (e,h) <- newEvent
  onEvent e $ writeRef r
  return (r,e,h)

onceRef :: UI (Ref (Maybe a),E a, H UI a)
onceRef = do
  r <- newRef Nothing
  (e,h) <- newEvent
  avail <- stepper True $ False <$ e
  onEvent (whenE avail e) $ \a -> do
    writeRef r $ Just a
    h a
  return (r,e,h)

-- }}}

-- Class Modifiers {{{

(#.) :: UI Element -> String -> UI Element
e #. c = e #.. [c]
infixl 8 #.

(#..) :: UI Element -> [String] -> UI Element
e #.. cs = e # set class_ cs
infixl 8 #..

(#.+) :: UI Element -> String -> UI Element
e #.+ c = e # modify class_ (L.insert c)
infixl 8 #.+

(#.++) :: UI Element -> [String] -> UI Element
e #.++ cs = e # modify class_ (L.union cs)
infixl 8 #.++

(#.-) :: UI Element -> String -> UI Element
e #.- c = e # modify class_ (filter (/= c))
infixl 8 #.-

(#.--) :: UI Element -> [String] -> UI Element
e #.-- cs = e # modify class_ (filter (not . (`elem` cs)))
infixl 8 #.--

(#./) :: UI Element -> (String,String) -> UI Element
e #./ (old,new) = e # modify class_ (map $ \s -> if s == old then new else s)
infixl 8 #./

(.#.) :: [a] -> [a] -> [a]
(.#.) = (++)
infixr 9 .#.

-- }}}

-- Style Modifiers {{{

(#@) :: UI Element -> [(String,String)] -> UI Element
e #@ ss = e # set style ss
infixl 8 #@

(#@+) :: UI Element -> (String,String) -> UI Element
e #@+ (k,a) = e # modify style (aListInsert k a)
infixl 8 #@+

(#@++) :: UI Element -> [(String,String)] -> UI Element
e #@++ ss = e # modify style (aListUnion ss)
infixl 8 #@++

(#@-) :: UI Element -> String -> UI Element
e #@- k = e # modify style (aListDelete k)
infixl 8 #@-

(#@--) :: UI Element -> [String] -> UI Element
e #@-- ss = e # modify style (aListDeleteAll ss)
infixl 8 #@--

(#@/) :: UI Element -> (String,Maybe String -> String) -> UI Element
e #@/ (k,f) = e & modify style go
  where
  go :: [(String,String)] -> [(String,String)]
  go ps = (k,f $ lookup k ps) : aListDelete k ps
infixl 8 #@/

aListInsert :: Eq k => k -> a -> [(k,a)] -> [(k,a)]
aListInsert k a ps =
  [ (k',b)
  | (k',a') <- ps
  , let b = cond a a' $ k == k'
  ]

aListUnion :: Eq k => [(k,a)] -> [(k,a)] -> [(k,a)]
aListUnion = \case
  []         -> id
  (k,a) : as -> aListInsert k a . aListUnion as

aListDelete :: Eq k => k -> [(k,a)] -> [(k,a)]
aListDelete k ps =
  [ (k',a)
  | (k',a) <- ps
  , k /= k'
  ]

aListDeleteAll :: Eq k => [k] -> [(k,a)] -> [(k,a)]
aListDeleteAll = \case
  []     -> id
  k : ks -> aListDelete k . aListDeleteAll ks

-- }}}

-- Application Combinators {{{

(##) :: a -> [a -> a] -> a
a ## fs = foldr (.) id fs a
infixl 8 ##

(&) :: a -> (a -> b) -> b
(&) = (#)
infixr 0 &
{-# INLINE (&) #-}

(&*) :: forall a. a -> [a -> a] -> a
(&*) = flip $ foldr (flip (.)) id
infixr 0 &*
{-# INLINE (&*) #-}

(.%) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.%) = (.) . (.)
infixr 7 .%

(%.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f %. g = \x y -> f (g x) (g y)
infixl 8 %.

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
infixl 4 <$$>

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<**>) = liftA2 (<*>)
infixl 4 <**>

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

(<&>) :: Functor f => f (a -> b) -> a -> f b
f <&> a = ($ a) <$> f
infixl 4 <&>

-- }}}

-- Conditional Combinators {{{

upon :: Bool -> (a -> a) -> a -> a
upon b f = if b
  then f
  else id

cond :: a -> a -> Bool -> a
cond t f = \case
  True -> t
  _    -> f

(?) :: a -> a -> Bool -> a
(?) = cond
infix 8 ?

-- }}}

-- Event / Behavior Combinators {{{

(#>>) :: UI Element -> (Element -> UI (E a),Element -> a -> UI void) -> UI Element
me #>> (mv,h) = do
  el <- me
  e  <- mv el
  onEvent e $ h el
  return el
infixl 8 #>>

(#>) :: UI Element -> (Element -> E a,Element -> a -> UI void) -> UI Element
me #> (e,h) = do
  el <- me
  on e el $ h el
  return el
infixl 8 #>

(#<) :: UI Element -> (Element -> B a,Element -> a -> UI void) -> UI Element
me #< (b,h) = do
  el <- me
  sink (mkWriteAttr $ \a e -> void $ h e a) (b el) $ return el
infixl 8 #<

(#<<) :: UI Element -> (Element -> UI (B a),Element -> a -> UI void) -> UI Element
me #<< (mb,h) = do
  el <- me
  b  <- mb el
  sink (mkWriteAttr $ \a e -> void $ h e a) b $ return el
infixl 8 #<<

(>>=@) :: B a -> (a -> UI b) -> UI (B b)
ba >>=@ f = do
  (e,h) <- newEvent
  onChanges ba $ f >=> h
  a <- f =<< currentValue ba
  stepper a e
infixl 1 >>=@

zipB :: B a -> B b -> B (a,b)
zipB = liftA2 (,)

unzipB :: B (a,b) -> (B a,B b)
unzipB ab = (fst <$> ab,snd <$> ab)

varyAttr :: (a -> ReadWriteAttr x i o) -> B a -> ReadWriteAttr x i o
varyAttr f b = mkReadWriteAttr g s
  where
  g x = do
    a <- currentValue b
    get' (f a) x
  s i x = do
    a <- currentValue b
    set' (f a) i x

-- }}}

-- Addtl Operators {{{

(#+>) :: UI Element -> [Element] -> UI Element
e #+> es = e #+ map return es
infixl 8 #+>

(#!) :: UI Element -> String -> UI Element
e #! s = e # set text s
infixl 8 #!

(.=) :: ReadWriteAttr x i o -> i -> UI x -> UI x
(.=) = set
infix 9 .=

(.?) :: x -> ReadWriteAttr x i o -> UI o
(.?) = flip get
infix 2 .?

-- }}}

-- Addtl Elements {{{

thead :: UI Element
thead = mkElement "thead"

tbody :: UI Element
tbody = mkElement "tbody"

head_ :: UI Element
head_ = windows getHead
  
body_ :: UI Element
body_ = windows getBody

script :: UI Element
script = mkElement "script"

cssPath :: String -> UI Element
cssPath p = UI.link ##
  [ rel   .= "stylesheet"
  , type_ .= "text/css"
  , href  .= p
  ]

jsPath :: String -> UI Element
jsPath p = script ##
  [ async   .= True
  , type_   .= "text/javascript"
  , charset .= "utf8"
  , src     .= p
  ]

jsScript :: JSFunction () -> UI Element
jsScript f = do
  cd <- renderFn f
  script # type_ .= "text/javascript" #! cd
  where
  renderFn :: JSFunction a -> UI String
  renderFn = liftIO . JS.toCode

-- }}}

-- JSON {{{

jsonWriteAttr :: ToJSON i => ReadWriteAttr x String o -> ReadWriteAttr x i o
jsonWriteAttr = bimapAttr toJSONStr id

toJSONStr :: ToJSON a => a -> String
toJSONStr = BS.unpack . encode

parseJSONStr :: FromJSON a => String -> Maybe a
parseJSONStr = decode . BS.pack

parseJSONValue :: FromJSON a => Value -> Maybe a
parseJSONValue = parseMaybe parseJSON

-- }}}

iota :: (Enum a, Num a) => a -> [a]
iota n = [0 .. pred n]

px :: Int -> String
px = (++ "px") . show

-- Safe list indexing
(?!) :: [a] -> Int -> Maybe a
(?!) = \case
  []     -> pure Nothing
  a : as -> \case
    0         -> Just a
    n | n > 0 -> as ?! pred n
    _         -> Nothing
infix 8 ?!

ffiReady :: FFI a => String -> a
ffiReady = ffi . wrapReady

wrapReady :: String -> String
wrapReady fn = "$(document).ready(function() { " ++ fn ++ ";})"

