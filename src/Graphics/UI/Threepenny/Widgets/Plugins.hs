{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.UI.Threepenny.Widgets.Plugins
  ( Plugin(..)
  , MonadUI(..)
  , plugin
  , pluginVendor
  , withPlugins
  , module Exports
  ) where

import Graphics.UI.Threepenny.Extra
import Foreign.JavaScript as Exports (JSObject)
import Foreign.RemotePtr  as Exports (Vendor,Coupon,RemotePtr)
import qualified Foreign.RemotePtr as Foreign

class Plugin p where
  initPlugin :: p -> UI ()

newtype WithPlugin p m a = WithPlugin
  { runWithPlugin :: m a
  } deriving (Functor,Applicative,Monad,MonadIO)

class MonadIO m => MonadUI m where
  liftUI :: UI a -> m a

instance MonadUI UI where
  liftUI = id

instance MonadUI m => MonadUI (WithPlugin p m) where
  liftUI = WithPlugin . liftUI

plugin :: (Plugin p, MonadUI m) => p -> WithPlugin p m a -> m a
plugin p m = do
  liftUI $ initPlugin p
  runWithPlugin m

pluginVendor :: (Plugin p, MonadUI m) => p -> (Vendor JSObject -> WithPlugin p m a) -> m a
pluginVendor p f = plugin p $ liftIO Foreign.newVendor >>= f

withPlugins :: MonadUI m => UI void -> m ()
withPlugins = liftUI . void

