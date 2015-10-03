
module Graphics.UI.Threepenny.Elements.ID where

import Graphics.UI.Threepenny.Attributes.Extra
import Graphics.UI.Threepenny.Extra
import Data.UUID
import Data.UUID.V4

newtype UID = UID
  { unUID :: String
  } deriving (Eq,Ord,Show)

newID :: UI UID
newID = liftIO $ shortenUUID <$> nextRandom

shortenUUID :: UUID -> UID
shortenUUID = UID . takeWhile (/= '-') . toString

uid :: MAttr Element UID
uid = strMAttr "id"
  & bimapAttr unUID (fmap UID)

identify :: Widget w => w -> UI UID
identify w = do
  i <- newID
  element w # set uid i
  return i

type Selector = String

selUID :: UID -> Selector
selUID = ('#':) . unUID

toVar :: UID -> String
toVar = unUID

byUID :: Identified a => a -> Selector
byUID = selUID . getUID

jsVar :: Identified a => a -> String
jsVar = (\p i -> p ++ "_" ++ i)
  <$> jsVarPrefix
  <*> toVar . getUID

class Identified a where
  getUID      :: a -> UID
  jsVarPrefix :: a -> String

