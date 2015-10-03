{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Control.Monad
import Graphics.UI.Threepenny.Extra
import qualified Graphics.UI.Threepenny as UI
import Data.IORef

run :: IO ()
run = startGUI defaultConfig
  { jsPort   = Just 10000
  , jsStatic = Just "static"
  } $ pure setup

setup :: UI ()
setup = void $ do
  hRef :: IORef [IO ()] <- liftIO $ newIORef []
  w <- window
  testBtn <- UI.button #! "Test"
  addBtn <- UI.button
    #! "Add Alert"
    #> ( UI.click
       , \_ _ -> liftIO $ do
         urs <- readIORef hRef
         let n = length urs + 1
         unreg <- register (UI.click testBtn) $ \_ ->
           runUI w $ runFunction $ ffi "alert(%1)" $ "Alert #" ++ show n
         writeIORef hRef (unreg:urs)
       )
  remBtn <- UI.button
    #! "Remove Alert"
    #> ( UI.click
       , \_ _ -> liftIO $ do
         urs <- readIORef hRef
         case urs of
           []   -> return ()
           unreg:rest -> do
             unreg
             writeIORef hRef rest
       )
  body_ #+
    [ UI.div #+
      [ element addBtn
      , element remBtn
      ]
    , UI.hr
    , UI.div #+
      [ element testBtn
      ]
    ]

