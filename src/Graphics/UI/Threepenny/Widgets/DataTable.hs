{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Threepenny.Widgets.DataTable where

import Prelude hiding (id,(.))
import Control.Category
import Graphics.UI.Threepenny.Attributes.Extra
import Graphics.UI.Threepenny.Elements.ID
import Graphics.UI.Threepenny.Extra hiding ((.=))
import qualified Graphics.UI.Threepenny as UI
import Control.Monad
import Foreign.JavaScript (JSObject)
import qualified Foreign.JavaScript.Marshal as JS
import qualified Foreign.RemotePtr as Foreign
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as BSIO
import Graphics.UI.Threepenny.Widgets.Plugins
import Graphics.UI.Threepenny.Action

data DataTable = DataTable
  { dtElement :: Element
  , dtUID     :: UID
  , dtRef     :: Ref JSObject
  , dtInitE   :: E ()
  , dtInitB   :: B Bool
  }

instance Widget DataTable where
  getElement = dtElement

instance Identified DataTable where
  getUID        = dtUID
  jsVarPrefix _ = "dt"

dtObject :: DataTable -> UI JSObject
dtObject = readRef . dtRef

class (ToJSON a, FromJSON a) => JSONRow a where
  colNames :: p a -> [String]

data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  }

instance ToJSON Person where
  toJSON p = object
    [ "first-name" .= firstName p
    , "last-name"  .= lastName  p
    , "age"        .= age       p
    ]

instance FromJSON Person where
  parseJSON = withObject "Person" $ \o -> Person
    <$> o .: "first-name"
    <*> o .: "last-name"
    <*> o .: "age"

instance JSONRow Person where
  colNames _ = ["first-name","last-name","age"]

data LazyT a = LazyT
  { lazyB  :: B (Maybe a)
  , readyE :: E ()
  }

data JSInit = JSInit
  { initB     :: B (Maybe JSObject)
  , initReady :: E ()
  }

refB :: Ref a -> Event void -> UI (B a)
refB r ev = do
  (e,h) <- newEvent
  onEvent ev $ \_ -> do
    a' <- readRef r
    h a'
  a <- readRef r
  stepper a e

jsObjectT :: JSFunction JSObject -> UI JSInit
jsObjectT f = do
  (r,e,h) <- onceRef
  runLater $ h =<< callFunction f
  b <- refB r e
  return $ JSInit b $ () <$ e

jsDataTable :: JSONRow a => [a] -> UI (JSInit,Element)
jsDataTable as = do
  t <- UI.table #. "display"
  i <- identify t
  lt <- jsObjectT
    $ ffi "$(%1).DataTable(%2)" (selUID i)
    $ object
      [ "data"    .= as
      , "columns" .=
        [ object [ "data" .= c ]
        | c <- cs
        ]
      ]
  return (lt,t)
  where
  cs = colNames as

testPeople :: [Person]
testPeople =
  [ Person "Kyle" "Carter" 27
  , Person "Toni" "Carter" 28
  ]

onInit :: JSInit -> (Element -> JSObject -> UI void) -> (Element -> UI (B (Maybe JSObject)),Element -> Maybe JSObject -> UI ())
onInit js f =
  ( pure $ return $ initB js
  , \e mo -> case mo of
    Just o -> void $ f e o
    _      -> return ()
  )

-- Plugin {{{

data DataTables = DataTables

instance Plugin DataTables where
  initPlugin _ = void $ do
    head_ #+ [ cssPath "static/css/jquery.dataTables.min.css" ]
    body_ #+ [ jsPath "static/js/jquery.dataTables.min.js" ]

mkDataTable :: (hdr -> Element -> UI Element) -> (row -> Int -> Element -> UI Element) -> [hdr] -> [row] -> UI DataTable
mkDataTable mkHdr mkEl hs rs = do
  t <- UI.table ##
    [ set class_ ["display"]
    ] #+
    [ thead #+
      [ UI.tr #+
        [ UI.th >>= mkHdr h
        | h <- hs
        ]
      ]
    , tbody #+
      [ UI.tr #+
        [ UI.td >>= mkEl r i
        | i <- iota $ length hs
        ]
      | r <- rs
      ]
    ]
  (r,e,h) <- newRefE =<< emptyJSObject
  b <- stepper False (True <$ e)
  i <- identify t
  initDataTable i h
  return $ DataTable t i r (() <$ e) b

initDataTable :: UID -> H UI JSObject -> UI ()
initDataTable i h = runLater $ h =<< callFunction (ffi "$(%1).DataTable()" $ selUID i)

emptyJSObject :: UI JSObject
emptyJSObject = callFunction $ ffi "{}"

-- }}}

-- Attributes {{{

dtAttr :: ToJSON a => String -> WriteAttr DataTable a
dtAttr a = writeOnly (data_ a)
  # mapAttrSrc dtElement
  # mapWriteAttr toJSONStr

dtBoolAttr :: String -> WriteAttr DataTable Bool
dtBoolAttr = dtAttr

autoWidth    = dtBoolAttr "auto-width"
info         = dtBoolAttr "info"
ordering     = dtBoolAttr "ordering"
paging       = dtBoolAttr "paging"
processing   = dtBoolAttr "processing"
lengthChange = dtBoolAttr "length-change"
scrollX      = dtBoolAttr "scroll-x"
scrollY      = dtBoolAttr "scroll-y"
searching    = dtBoolAttr "searching"

{-
tableData :: ReadAttr DataTable Value
tableData = mkReadAttr $ \dt -> callFunction
  $ ffi "%1.rows().data()"
  $ uuidSelector $ dtUID dt
-}

-- }}}

runDT :: IO ()
runDT = startGUI defaultConfig
  { jsPort       = Just 10000
  , jsCustomHTML = Just "index.html"
  , jsInclude    = []
  , jsStatic     = Just "static"
  } $ pure dtSetup

container :: UI Element
container = UI.div #. "container"

content_ :: UI Element
content_ = UI.div #. "content"

dtSetup :: UI ()
dtSetup = plugin DataTables
  $ withPlugins $ do
    addCSSFile "containers.css"
    e <- UI.div #+
      [ UI.div #! "Element 1"
      , UI.div #! "Element 2"
      , UI.div #! "Element 3"
      , UI.div #! "Element 4"
      ]
    body_ #+
      [ container #+
        [ content_ #+
          [ element e
          , UI.hr
          , UI.div #! "Children Test"
          , UI.hr
          , do es  <- parseElements "<div>Div</div><span>Span</span>"
               es' <- filterElements es "div"
               UI.div #+> es'
          ]
        ]
      ]

