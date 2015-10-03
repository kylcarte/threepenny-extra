{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Graphics.UI.Threepenny.DragNDrop.Extra
  ( module Graphics.UI.Threepenny.DragNDrop.Extra
  , module Graphics.UI.Threepenny.Table
  ) where

import Control.Monad
import Graphics.UI.Threepenny.Action
import Graphics.UI.Threepenny.Table
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

type DropChoices = Element -> [Element] -> UI [Element]

allInSet :: DropChoices
allInSet e es = return $ e : es

onlyOthers :: DropChoices
onlyOthers _ = return

butFirst :: (Either Element Element -> UI a) -> DropChoices -> DropChoices
butFirst f choose e es = do
  void $ f $ Left e
  mapM_ (f . Right) es
  choose e es

dragDropSet :: a
  -> [(Element,a)]
  -> DropChoices
  -> (Content a -> Content a -> UI ())
  -> UI ()
dragDropSet def ps drp act = do
  dragRef <- newRef . uncurry Content =<< (,) <$> UI.new <*> newRef def
  let allElems = map fst ps
  forM_ (foci ps) $ \((e,a),rest) -> do
    eContent <- Content e <$> newRef a
    element e # set UI.draggable True
    on UI.dragStart e $ \_ -> do
      mapM_ (set UI.droppable True . element) =<< drp e (map fst rest)
      writeRef dragRef eContent
    on UI.drop e $ \_ -> do
      forM_ allElems $ set UI.droppable False . element
      flip act eContent =<< readRef dragRef

type TblElements = (Element,[RowElements])
type RowElements = (Element,[Element])

renderRowsWith :: forall f as. KnownLength as
  => Rows f as
  -> (forall a. Index as a -> Maybe (f a) -> UI Element -> UI Element)
  -> (Int -> Maybe (Row f as) -> [Element] -> UI [Element])
  -> UI TblElements
renderRowsWith t mkCell mkRow = do
  hs  <- buildHeader len
  rs  <- mapM (uncurry buildRow) $ zip [1..] t
  let hrs = hs : rs
  tbl <- UI.table # set children (map fst hrs)
  return (tbl,hrs)
  where
  buildHeader :: Row g as -> UI RowElements
  buildHeader xs = do
    hs <- mkRow 0 Nothing =<< foldrMWithIndex'
      (\colIx _ cs -> do
        c <- mkCell colIx Nothing UI.th
        return $ c : cs
      ) [] xs
    r <- UI.tr # set children hs
    return (r,hs)
  buildRow :: Int -> Row f as -> UI RowElements
  buildRow rowIx pr = do
    cs <- mkRow rowIx (Just pr) =<< foldrMWithIndex'
      (\colIx a cs -> do
        c <- mkCell colIx (Just a) UI.td
        return $ c : cs
      ) [] pr
    r <- UI.tr # set children cs
    return (r,cs)

