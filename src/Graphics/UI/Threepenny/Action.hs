{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.UI.Threepenny.Action where

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.DragNDrop
import qualified Graphics.UI.Threepenny as UI
import Data.IORef
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.String (fromString)

-- Ref {{{

type Ref = IORef

newRef :: a -> UI (Ref a)
newRef = liftIO . newIORef

readRef :: Ref a -> UI a
readRef = liftIO . readIORef

writeRef :: Ref a -> a -> UI ()
writeRef x = liftIO . writeIORef x

modifyRef :: Ref a -> (a -> a) -> UI ()
modifyRef x = liftIO . modifyIORef x

swapRefs :: Ref a -> Ref a -> UI (a,a)
swapRefs refA refB = do
  a <- readRef refA
  b <- readRef refB
  writeRef refA b
  writeRef refB a
  return (a,b)

-- }}}

-- Action {{{

type Action = UI ()

noOpAction :: UI (Ref Action)
noOpAction = newRef $ return ()

runAction :: Ref Action -> UI ()
runAction = join . readRef

action :: (element -> Event a) -> element -> UI (Ref Action)
action f e = do
  x <- noOpAction
  on f e $ \_ -> runAction x
  return x

withAction :: (element -> Event a) -> element -> (Ref Action -> UI b) -> UI b
withAction f e g = action f e >>= g

-- }}}

-- Reaction {{{

type Reaction a = a -> Action

noOpReaction :: UI (Ref (Reaction a))
noOpReaction = newRef $ \_ -> return ()

runReaction :: Ref (Reaction a) -> a -> UI ()
runReaction x a = join $ ($ a) <$> readRef x

reaction :: (element -> Event a) -> element -> UI (Ref (Reaction a))
reaction f e = do
  x <- noOpReaction
  on f e $ runReaction x
  return x

withReaction :: (element -> Event a) -> element -> (Ref (Reaction a) -> UI b) -> UI b
withReaction f e g = reaction f e >>= g

-- }}}

-- Content {{{

data Content a = Content
  { contentHolder :: Element
  , contentRef    :: Ref a
  }

traverseHolder :: Applicative f => (Element -> f Element) -> Content a -> f (Content a)
traverseHolder f (Content e r) = Content <$> f e <*> pure r

emptyContent :: Monoid a => UI (Content a)
emptyContent = Content <$> UI.new <*> newRef mempty

swapContents :: (a -> Element -> UI Element) -> Content a -> Content a -> UI (Element,Element)
swapContents f (Content elA refA) (Content elB refB) = do
  (a,b) <- swapRefs refA refB
  (,) <$> f b elA <*> f a elB

-- }}}

foci :: [a] -> [(a,[a])]
foci = go id
  where
  go :: ([a] -> [a]) -> [a] -> [(a,[a])]
  go k = \case
    []     -> []
    a : as -> (a,k as) : go ((a :) . k) as

encodeDragData :: ToJSON a => a -> DragData
encodeDragData = BS.foldr (:) "" . encode

decodeDragData :: FromJSON a => DragData -> Maybe a
decodeDragData = decode . fromString

{-
dragNDrop :: Ref (Content a) -> (Content a -> Content a -> UI ()) -> Element -> a -> UI Element
dragNDrop ref to e a = do
  here <- Content e <$> newRef a
  on UI.dragStart e $ \_ -> do
    writeRef ref here
  on UI.drop e $ \_ -> do
    there <- readRef ref
    there `to` here
  return e
-}

dragContent :: ToJSON a => Content a -> UI ()
dragContent (Content e r) = on (UI.dragStart) e $ \_ -> do
  content <- readRef r
  element e # set UI.dragData (encodeDragData content)

-- Map Events {{{

data MapE k a
  = Insert k a
  | Delete k
  | Union        (M k a)
  | Difference   (M k a)
  | Intersection (M k a)
  deriving (Eq,Ord,Show,Read)

newtype M k a = M
  { getM :: Map k a
  } deriving (Eq,Ord,Show,Read)

instance (ToJSON k, ToJSON a) => ToJSON (M k a) where
  toJSON = toJSON . M.assocs . getM

instance (Ord k, FromJSON k, FromJSON a) => FromJSON (M k a) where
  parseJSON v = M . M.fromList <$> parseJSON v

instance (ToJSON k, ToJSON a) => ToJSON (MapE k a) where
  toJSON d = object [ key .= val ]
    where
    (key,val) = case d of
      Insert     k a -> ("insert"       , toJSON (k,a))
      Delete     k   -> ("delete"       , toJSON  k   )
      Union        m -> ("union"        , toJSON  m   )
      Difference   m -> ("difference"   , toJSON  m   )
      Intersection m -> ("intersection" , toJSON  m   )

instance (Ord k, FromJSON k, FromJSON a) => FromJSON (MapE k a) where
  parseJSON = withObject "MapE" $ \o -> msum
    [ uncurry
      Insert       <$> o .: "insert"
    , Delete       <$> o .: "delete"
    , Union        <$> o .: "union"
    , Difference   <$> o .: "difference"
    , Intersection <$> o .: "intersection"
    ]

-- }}}

