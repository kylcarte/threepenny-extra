{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.UI.Threepenny.Attributes.Extra where

import           Graphics.UI.Threepenny.Core hiding (text,children)
import qualified Graphics.UI.Threepenny.Core as Core
import           Foreign.JavaScript (FromJS,JSObject)
import           Data.Maybe (fromMaybe)
import           Text.Read (readMaybe)
import           Control.Arrow (first)
import           Control.Monad ((>=>))
import           Data.Aeson (Value,FromJSON(..),ToJSON(..))
import           Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson                  as JSON
import qualified Data.Text                   as T
import qualified Data.List                   as L
import           Data.Char

modify :: ReadWriteAttr x i o -> (o -> i) -> UI x -> UI x
modify a f m = do
  o <- get a =<< m
  m # set a (f o)

-- Addtl Attributes {{{

data_ :: String -> MAttr Element String
data_ = strMAttr . ("data-" ++)

charset :: MAttr Element String
charset = strMAttr "charset"

parsedChildren :: WriteAttr Element String
parsedChildren = bindAttrWrite parseElements
  $ writeOnly
  $ children

parsedChildrenSelect :: String -> WriteAttr Element String
parsedChildrenSelect sel = bindAttrWrite (flip parseElementsSelect sel)
  $ writeOnly
  $ children

parseElements :: String -> UI [Element]
parseElements s = fromJQuery =<<
  callFunction (ffi "$(%1)" s)

parseElementsSelect :: String -> String -> UI [Element]
parseElementsSelect s sel = parseElements s @##@ ("filter(%1)",Arg sel)

filterElements :: [Element] -> String -> UI [Element]
filterElements es sel = return es @##@ ("filter(%1)",Arg sel)

jQuery :: (JQueryArg a, FFIArgs f args (a -> JSFunction r), JQueryRes r res)
  => String -> args -> UI a -> UI res
jQuery f args ma = do
  a <- ma
  r <- callFunction
    $ withArgs
    ( ffi $ unwords
    [ "$(%"
    , show n
    , ")."
    , f
    ]
    ) args a
  fromJQuery r
  where
  n = numArgs args + 1

jQueryElement :: (FFIArgs f args (Element -> JSFunction r), JQueryRes r res)
  => String -> args -> UI Element -> UI res
jQueryElement = jQuery

jQueryElements :: (FFIArgs f args ([Element] -> JSFunction r), JQueryRes r res)
  => String -> args -> UI [Element] -> UI res
jQueryElements = jQuery

(@#@) :: (JQueryRes r res, FFIArgs f args (Element -> JSFunction r))
  => UI Element -> (String,args) -> UI res
me @#@ (f,args) = jQueryElement f args me
infixl 8 @#@

(@##@) :: (JQueryRes r res, FFIArgs f args ([Element] -> JSFunction r))
  => UI [Element] -> (String,args) -> UI res
mes @##@ (f,args) = jQueryElements f args mes
infixl 8 @##@

runFFI :: UI (JSFunction a) -> UI a
runFFI = (>>= callFunction)

ffiArgs :: forall f args res. FFIArgs f args (JSFunction res) => String -> args -> JSFunction res
ffiArgs f args = withArgs (ffi f :: f) args

class ToJS a => JQueryArg a
instance JQueryArg String
instance JQueryArg Element
instance JQueryArg [Element]

class FromJS a => JQueryRes a r | r -> a where
  fromJQuery :: a -> UI r

instance JQueryRes JSObject Element where
  fromJQuery = fromJSObject

instance JQueryRes [JSObject] [Element] where
  fromJQuery = mapM fromJSObject

class NumArgs a where
  numArgs :: a -> Int

instance NumArgs (Arg a) where
  numArgs _ = 1

instance NumArgs (a,b) where
  numArgs _ = 2

instance NumArgs (a,b,c) where
  numArgs _ = 3

instance NumArgs (a,b,c,d) where
  numArgs _ = 4

class (FFI f, NumArgs args, FFI res) => FFIArgs f args res | args f -> res, f res -> args, args res -> f where
  withArgs :: f -> args -> res

newtype Arg a = Arg a

pattern Args :: args -> args
pattern Args as = as

instance (ToJS a, FFI f) => FFIArgs (a -> f) (Arg a) f where
  withArgs f (Arg a) = f a

instance (ToJS a, ToJS b, FFI f) => FFIArgs (a -> b -> f) (a,b) f where
  withArgs f (a,b) = f a b

instance (ToJS a, ToJS b, ToJS c, FFI f) => FFIArgs (a -> b -> c -> f) (a,b,c) f where
  withArgs f (a,b,c) = f a b c

instance (ToJS a, ToJS b, ToJS c, ToJS d, FFI f) => FFIArgs (a -> b -> c -> d -> f) (a,b,c,d) f where
  withArgs f (a,b,c,d) = f a b c d

-- }}}

-- Builtin Attributes + Read {{{

text :: Attr Element String
text = mkReadWriteAttr getTxt setTxt
  where
  getTxt   e = callFunction $ ffi "$(%1).text()"   e
  setTxt s e = runFunction  $ ffi "$(%1).text(%2)" e s

children :: Attr Element [Element]
children = mkReadWriteAttr getChildren (set' Core.children)
  where
  getChildren e = callFunction (ffi "$(%1).children()" e)
    >>= mapM fromJSObject

class_ :: Attr Element [String]
class_ = attrDefault []
  $ mapMAttr words unwords
  $ strMAttr "class"

style :: Attr Element [(String,String)]
style = attrDefault []
  $ mapMAttr splitStyles joinStyles
  $ strMAttr "style"

joinStyles :: [(String,String)] -> String
joinStyles = L.intercalate ";" . map (joinPair ":")

splitStyles :: String -> [(String,String)]
splitStyles = map (breakWith $ sepWords ":") . breakAllWith (sepWords ";")

action              =   strMAttr "action"
align               =   strMAttr "align"
alink               =   strMAttr "alink"
alt                 =   strMAttr "alt"
altcode             =   strMAttr "altcode"
archive             =   strMAttr "archive"
async               = emptyMAttr "async"
background          =   strMAttr "background"
base                =   strMAttr "base"
bgcolor             =   strMAttr "bgcolor"
border              =   intMAttr "border"
bordercolor         =   strMAttr "bordercolor"
cellpadding         =   intMAttr "cellpadding"
cellspacing         =   intMAttr "cellspacing"
checked_            = emptyMAttr "checked"
clear_              =   strMAttr "clear"
code_               =   strMAttr "code"
codebase            =   strMAttr "codebase"
color               =   strMAttr "color"
cols                =   strMAttr "cols"
colspan             =   intMAttr "colspan"
compact             = emptyMAttr "compact"
content             =   strMAttr "content"
coords              =   strMAttr "coords"
enctype             =   strMAttr "enctype"
face                =   strMAttr "face"
for_                =   strMAttr "for"
frameborder         =   intMAttr "frameborder"
height              =   intMAttr "height"
href                =   strMAttr "href"
hspace              =   intMAttr "hspace"
httpequiv           =   strMAttr "http-equiv"
id_                 =   strMAttr "id"
ismap               = emptyMAttr "ismap"
lang                =   strMAttr "lang"
marginheight        =   intMAttr "marginheight"
marginwidth         =   intMAttr "marginwidth"
maxlength           =   intMAttr "maxlength"
method              =   strMAttr "method"
multiple            = emptyMAttr "multiple"
name                =   strMAttr "name"
nohref              = emptyMAttr "nohref"
noresize            = emptyMAttr "noresize"
noshade             = emptyMAttr "noshade"
nowrap              = emptyMAttr "nowrap"
rel                 =   strMAttr "rel"
rev                 =   strMAttr "rev"
rows                =   strMAttr "rows"
rowspan             =   intMAttr "rowspan"
rules               =   strMAttr "rules"
scrolling           =   strMAttr "scrolling"
selected            = emptyMAttr "selected"
shape               =   strMAttr "shape"
size                =   strMAttr "size"
src                 =   strMAttr "src"
target              =   strMAttr "target"
text_               =   strMAttr "text"
type_               =   strMAttr "type"
title__             =   strMAttr "title"
usemap              =   strMAttr "usemap"
valign              =   strMAttr "valign"
version             =   strMAttr "version"
vlink               =   strMAttr "vlink"
vspace              =   intMAttr "vspace"
width               =   intMAttr "width"

-- }}}

-- Attr builders {{{

type MAttr x a = ReadWriteAttr x a (Maybe a)

strMAttr :: String -> MAttr Element String
strMAttr nm = mkReadWriteAttr getStr setStr
  where
  getStr   e = do
    b <- get (hasAttr nm) e
    if not b
      then return Nothing
      else do
        v <- callFunction $ ffi "$(%1).attr(%2)" e nm
        case v of
          JSON.String t -> return $ Just $ T.unpack t
          _             -> return Nothing
  setStr s e = runFunction $ ffi "$(%1).attr(%2,%3)" e nm s

intMAttr :: String -> MAttr Element Int
intMAttr = mapReadWriteAttr (>>= readMaybe) show . strMAttr

emptyMAttr :: String -> Attr Element Bool
emptyMAttr nm = fromJQueryProp nm (== JSON.Bool True) JSON.Bool

hasAttr :: String -> ReadAttr Element Bool
hasAttr a = mapReadAttr (fromMaybe False)
  $ jsonReadAttr
  $ ffi "$(%2).is(%1)"
  $ "[" ++ a ++ "]"

jsonReadWriteAttr :: FromJSON o => (x -> JSFunction Value) -> (x -> i -> JSFunction ()) -> ReadWriteAttr x i (Maybe o)
jsonReadWriteAttr g s = mkReadWriteAttr (fmap (parseMaybe parseJSON) . callFunction . g) $ \i x -> runFunction $ s x i

jsonReadAttr :: FromJSON o => (x -> JSFunction Value) -> ReadAttr x (Maybe o)
jsonReadAttr g = mkReadAttr $ fmap (parseMaybe parseJSON) . callFunction . g

-- }}}

-- Attr Modifiers {{{

attrDefault :: a -> ReadWriteAttr x b (Maybe a) -> ReadWriteAttr x b a
attrDefault = mapReadAttr . fromMaybe

mapAttrRead :: ((x -> UI o) -> x -> UI o') -> ReadWriteAttr x i o -> ReadWriteAttr x i o'
mapAttrRead k = mkReadWriteAttr <$> k . get' <*> set'

mapAttrWrite :: ((i -> x -> UI ()) -> i' -> x -> UI ()) -> ReadWriteAttr x i o -> ReadWriteAttr x i' o
mapAttrWrite k = mkReadWriteAttr <$> get' <*> k . set'

mapAttrSrc :: (x' -> x) -> ReadWriteAttr x i o -> ReadWriteAttr x' i o
mapAttrSrc f (ReadWriteAttr g s) = ReadWriteAttr
  { get' = g . f
  , set' = \i -> s i . f
  }

bindAttrRead :: (o -> UI o') -> ReadWriteAttr x i o -> ReadWriteAttr x i o'
bindAttrRead f a = mkReadWriteAttr
  (get' a >=> f)
  (set' a)

bindAttrWrite :: (i -> UI i') -> ReadWriteAttr x i' o -> ReadWriteAttr x i o
bindAttrWrite f a = mkReadWriteAttr (get' a) $ \i x -> do
  i' <- f i
  set' a i' x

mapMAttr :: (a -> a') -> (a' -> a) -> MAttr x a -> MAttr x a'
mapMAttr f = mapReadWriteAttr (fmap f)

mapReadWriteAttr :: (o -> o') -> (i' -> i) -> ReadWriteAttr x i o -> ReadWriteAttr x i' o'
mapReadWriteAttr f g = mkReadWriteAttr
  <$> (fmap f .) . get'
  <*> (. g)      . set'

mapReadAttr :: (o -> o') -> ReadWriteAttr x i o -> ReadWriteAttr x i o'
mapReadAttr f = mkReadWriteAttr
  <$> (fmap f .) . get'
  <*> set'

mapWriteAttr :: (i' -> i) -> ReadWriteAttr x i o -> ReadWriteAttr x i' o
mapWriteAttr f = mkReadWriteAttr
  <$> get'
  <*> (. f) . set'

writeOnly :: ReadWriteAttr x i o -> WriteAttr x i
writeOnly = bimapAttr id $ pure ()

attrB :: (a -> ReadWriteAttr x i o) -> Behavior a -> ReadWriteAttr x i o
attrB f ba = mkReadWriteAttr
  (\x -> do
    a <- currentValue ba
    get' (f a) x
  )
  (\i x -> do
    a <- currentValue ba
    set' (f a) i x
  )
 
-- }}}

-- List helpers {{{

splitAll :: (a -> Bool) -> [a] -> [[a]]
splitAll pr as = case L.break pr as of
  (r,[]   ) -> [r]
  (r,a:as') -> r : onHead (a:) (splitAll pr as')

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll pr = onTail (map $ drop 1) . splitAll pr

onHead :: (a -> a) -> [a] -> [a]
onHead f = \case
  []   -> []
  a:as -> f a : as

onTail :: ([a] -> [a]) -> [a] -> [a]
onTail f = \case
  []   -> []
  a:as -> a : f as

breakOn :: Eq a => [a] -> [a] -> [[a]]
breakOn = breakAllWith . prefixes

sepWords :: String -> String -> Maybe String
sepWords sep = \case
  s    | Just s' <- prefixes sep s
      -> Just $ dropWhile isSpace s'
  c:s' | isSpace c
      -> sepWords sep s'
  _   -> Nothing

breakAllWith :: ([a] -> Maybe [a]) -> [a] -> [[a]]
breakAllWith f = \case
  [] -> []
  l  -> case breakWith f l of
    (r,[])   -> [r]
    (r,a:as) -> r : onHead (a:) (breakAllWith f as)

prefixes :: Eq a => [a] -> [a] -> Maybe [a]
prefixes = \case
  []     -> Just
  p : pr -> \case
    a : as | p == a
      -> prefixes pr as
    _ -> Nothing

breakOn1 :: Eq a => [a] -> [a] -> ([a],[a])
breakOn1 = breakWith . prefixes

joinPair :: [a] -> ([a],[a]) -> [a]
joinPair s (a,b) = a ++ s ++ b

breakWith :: ([a] -> Maybe [a]) -> [a] -> ([a],[a])
breakWith f = \case
  []       -> ([],[])
  l@(a:as) -> case f l of
    Just as' -> ([],as')
    _        -> first (a:) $ breakWith f as

-- }}}

