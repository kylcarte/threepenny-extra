{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
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

module Graphics.UI.Threepenny.Table where

import Control.Arrow
import Control.Monad
import Graphics.UI.Threepenny.Core hiding (Const(..))
import Data.Monoid ((<>))
import Data.Type.Equality
import GHC.Exts (Constraint)

type Ø    = '[]
type (:<) = '(:)
infixr 5 :<

type Length = Row (Const ())

class LengthC as => KnownLength (as :: [k]) where
  type LengthC (as :: [k]) :: Constraint
  len :: Length as

instance KnownLength (Ø :: [k]) where
  type LengthC (Ø :: [k]) = (() :: Constraint)
  len = Ø

instance KnownLength as => KnownLength (a :< as :: [k]) where
  type LengthC (a :< as :: [k]) = KnownLength as
  len = C () :< len

data Row (f :: k -> *) :: [k] -> * where
  Ø    :: Row f Ø
  (:<) :: !(f a) -> !(Row f as) -> Row f (a :< as)

type Tuple = Row Id
type Rows f as = [Row f as]
type Columns f = Row (Comp [] f)

instance Show (Row f Ø) where
  showsPrec _ _ = showString "<||>"

instance (Show (f a), Show (Row f as)) => Show (Row f (a :< as)) where
  showsPrec _ (a :< as) =
      showString "<|"
    . shows a
    . whenPeek (/= '|') (showChar ',')
    . drop 2
    . shows as
    where
    whenPeek :: (Char -> Bool) -> ShowS -> ShowS
    whenPeek pr f inp = case inp of
      c:_ | pr c -> f inp
      _          -> inp

single :: f a -> Row f '[a]
single = (:< Ø)

(<:) :: f a -> f b -> Row f '[a,b]
a <: b = a :< single b
infixl 6 <:

head' :: Row f (a :< as) -> f a
head' (a :< _) = a

tail' :: Row f (a :< as) -> Row f as
tail' (_ :< as) = as

data Split :: [k] -> [k] -> k -> [k] -> * where
  SplitZ :: Split (a :< aft) Ø a aft
  SplitS :: !(Split as bef a aft) -> Split (b :< as) (b :< bef) a aft

data Index :: [k] -> k -> * where
  Z :: Index (a :< as) a
  S :: !(Index as a) -> Index (b :< as) a

index :: Index as a -> Row f as -> f a
index = \case
  Z   -> head'
  S x -> index x . tail'

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  Ø         ++ bs = bs
  (a :< as) ++ bs = a :< (as ++ bs)
infixr 5 ++

append' :: Row f as -> Row f bs -> Row f (as ++ bs)
append' = \case
  Ø       -> id
  a :< as -> (a :<) . append' as

split' :: Split as bef a aft -> Row f as -> (Row f bef,f a,Row f aft)
split' = \case
  SplitZ   -> (,,) Ø <$> head' <*> tail'
  SplitS s -> \(b :< as) -> let
    (bef,a,aft) = split' s as
    in (b :< bef,a,aft)

map' :: (forall a. f a -> g a) -> Row f as -> Row g as
map' f = \case
  Ø       -> Ø
  a :< as -> f a :< map' f as

sequence' :: Monad m => Row m as -> m (Tuple as)
sequence' = \case
  Ø       -> return Ø
  m :< ms -> liftM2 (:<) (return <$> m) $ sequence' ms

mapWithIndex' :: forall f g as. (forall a. Index as a -> f a -> g a) -> Row f as -> Row g as
mapWithIndex' f = \case
  Ø       -> Ø
  a :< as -> f Z a :< mapWithIndex' (f . S) as

foldMap' :: Monoid m => (forall a. f a -> m) -> Row f as -> m
foldMap' f = \case
  Ø       -> mempty
  a :< as -> f a <> foldMap' f as

foldMapWithIndex' :: Monoid m => (forall a. Index as a -> f a -> m) -> Row f as -> m
foldMapWithIndex' f = \case
  Ø       -> mempty
  a :< as -> f Z a <> foldMapWithIndex' (f . S) as

foldMapM' :: (Monad m, Monoid r) => (forall a. f a -> m r) -> Row f as -> m r
foldMapM' f = \case
  Ø       -> return mempty
  a :< as -> liftM2 (<>) (f a) $ foldMapM' f as

foldlM' :: Monad m => (forall a. b -> f a -> m b) -> b -> Row f as -> m b
foldlM' f b = \case
  Ø       -> return b
  a :< as -> do
    b' <- f b a
    foldlM' f b' as

foldr' :: (forall a. f a -> b -> b) -> b -> Row f as -> b
foldr' f b = \case
  Ø       -> b
  a :< as -> f a $ foldr' f b as

foldrM' :: Monad m => (forall a. f a -> b -> m b) -> b -> Row f as -> m b
foldrM' f b = \case
  Ø       -> return b
  a :< as -> do
    b' <- foldrM' f b as
    f a b'

foldrMWithIndex' :: Monad m => (forall a. Index as a -> f a -> b -> m b) -> b -> Row f as -> m b
foldrMWithIndex' f b = \case
  Ø       -> return b
  a :< as -> do
    b' <- foldrMWithIndex' (f . S) b as
    f Z a b'

traverse' :: Applicative h
  => (forall a. f a -> h (g a))
  -> Row f as
  -> h (Row g as)
traverse' f = \case
  Ø       -> pure Ø
  a :< as -> (:<) <$> f a <*> traverse' f as

traverseWithIndex' :: Applicative h
  => (forall a. Index as a -> f a -> h (g a))
  -> Row f as
  -> h (Row g as)
traverseWithIndex' f = \case
  Ø       -> pure Ø
  a :< as -> (:<) <$> f Z a <*> traverseWithIndex' (f . S) as

toList :: (forall a. f a -> r) -> Row f as -> [r]
toList f = foldMap' $ (:[]) . f

toListWithIndex :: (forall a. Index as a -> f a -> r) -> Row f as -> [r]
toListWithIndex f = foldMapWithIndex' $ \x -> (:[]) . f x

prodRecR :: p Ø -> (forall x xs. f x -> p xs -> p (x :< xs))
  -> Row f as -> p as
prodRecR z s = \case
  Ø       -> z
  a :< as -> s a $ prodRecR z s as

snoc :: Row f as -> f a -> Row f (as :> a)
snoc = \case
  Ø       -> single
  b :< as -> (b :<) . snoc as

consIL :: Row f (a :< as) -> (a :< as) :~: (Init' a as :> Last' a as)
consIL (_ :< as) = case as of
  Ø      -> Refl
  (:<){} -> case consIL as of
    Refl -> Refl

snocHT :: Row f as -> f a -> (as :> a) :~: (Head' as a :< Tail' as a)
snocHT as a = case as of
  Ø       -> Refl
  _ :< bs -> case snocHT bs a of
    Refl -> Refl

ht_il :: Row f (a :< as) -> Row f (Init' a as :> Last' a as)
ht_il as@(a :< as') = case as' of
  Ø      -> as
  (:<){} -> a :< ht_il as'
{-# INLINE ht_il #-}

init' :: Row f (a :< as) -> Row f (Init' a as)
init' (a :< as) = case as of
  Ø      -> Ø
  (:<){} -> a :< init' as

last' :: Row f (a :< as) -> f (Last' a as)
last' (a :< as) = case as of
  Ø      -> a
  (:<){} -> last' as

type family (as :: [k]) :> (a :: k) :: [k] where
  Ø         :> a = '[a]
  (b :< as) :> a = b :< as :> a
infixl 6 :>

type family Head (as :: [k]) :: Maybe k where
  Head  Ø        = Nothing
  Head (a :< as) = Just (Head' (Init' a as) (Last' a as))

type family Head' (as :: [k]) (a :: k) :: k where
  Head'  Ø        a = a
  Head' (b :< as) a = b

type family Tail (as :: [k]) :: Maybe [k] where
  Tail  Ø        = Nothing
  Tail (a :< as) = Just (Tail' (Init' a as) (Last' a as))

type family Tail' (as :: [k]) (a :: k) :: [k] where
  Tail'  Ø        a = Ø
  Tail' (b :< as) a = as :> a

type family Init (as :: [k]) :: Maybe [k] where
  Init  Ø        = Nothing
  Init (a :< as) = Just (Init' a as)

type family Init' (a :: k) (as :: [k]) :: [k] where
  Init' a  Ø        = Ø
  Init' a (b :< as) = a :< Init' b as

type family Last (as :: [k]) :: Maybe k where
  Last  Ø        = Nothing
  Last (a :< as) = Just (Last' a as)

type family Last' (a :: k) (as :: [k]) :: k where
  Last' a  Ø        = a
  Last' a (b :< as) = Last' b as

type family Reverse (as :: [k]) :: [k] where
  Reverse  Ø        = Ø
  Reverse (a :< as) = Reverse as :> a

type family (f :: k -> l) <$> (a :: Maybe k) :: Maybe l where
  f <$>  Nothing = Nothing
  f <$> (Just a) = Just (f a)
infixl 4 <$>

type family (f :: Maybe (k -> l)) <*> (a :: Maybe k) :: Maybe l where
  Nothing <*> a = Nothing
  Just f  <*> a = f <$> a
infixl 4 <*>

toColumns :: KnownLength as => Rows f as -> Columns f as
toColumns (r :: Rows f as) = go (len :: Length as) r
  where
  go :: Length bs -> Rows f bs -> Columns f bs
  go = \case
    Ø      -> pure Ø
    _ :< l -> (:<)
      <$> Comp . map head'
      <*> go l . map tail'

column :: Index as a -> Rows f as -> [f a]
column = map . index

addColumn :: [f a] -> Rows f as -> Rows f (a :< as)
addColumn = zipWith (:<)

deleteColumn :: Split as bef a aft -> Rows f as -> ([f a],Rows f (bef ++ aft))
deleteColumn s rs = unzip
  [ (a,append' bef aft)
  | (bef,a,aft) <- map (split' s) rs
  ]

data Rs (f :: k -> *) :: [k] -> N -> * where
  ØR   :: Rs f as Ze
  (:-) :: Row f as -> Rs f as n -> Rs f as (Su n)
infixr 5 :-

data Cs (f :: k -> *) :: N -> [k] -> * where
  ØC   :: Cs f n Ø
  (:|) :: Col n (f a) -> Cs f n as -> Cs f n (a :< as)
infixr 5 :|

-- Col {{{

data Col :: N -> * -> * where
  Nil  :: Col Ze a
  (:*) :: a -> Col n a -> Col (Su n) a
infixr 5 :*

instance Functor (Col n) where
  fmap f = \case
    Nil     -> Nil
    a :* as -> f a :* (f <$> as)

instance KnownNumber n => Applicative (Col n) where
  pure = go (nat :: Nat n)
    where
    go :: Nat x -> a -> Col x a
    go = \case
      Z_   -> pure Nil
      S_ x -> (:*) <*> go x
  (<*>) = \case
    Nil     -> pure Nil
    f :* fs -> \case
      a :* as -> f a :* (fs <*> as)
      _       -> imp

diagonal :: Col n (Col n a) -> Col n a
diagonal = \case
  Nil            -> Nil
  (a :* _) :* cs -> a :* diagonal cs'
    where
    cs' = (\(_ :* as) -> as) <$> cs
  _              -> imp

instance KnownNumber n => Monad (Col n) where
  c >>= f = diagonal $ f <$> c

-- }}}

splitCol :: Rs f (a :< as) n -> (Col n (f a),Rs f as n)
splitCol = \case
  ØR      -> (Nil,ØR)
  r :- rs -> (head' r :*) *** (tail' r :-) $ splitCol rs

toCols :: forall f as n. KnownLength as => Rs f as n -> Cs f n as
toCols = \case
  ØR -> go (len :: Length as)
    where
    go :: Length bs -> Cs f Ze bs
    go = \case
      Ø      -> ØC
      _ :< l -> Nil :| go l
  rs@(r :- _) -> case r of
    Ø      -> ØC
    (:<){} -> uncurry (:|) $ second toCols $ splitCol rs

data N
  = Ze
  | Su N
  deriving (Eq,Ord,Show)

data Nat :: N -> * where
  Z_ :: Nat Ze
  S_ :: !(Nat n) -> Nat (Su n)

class NumberC n => KnownNumber (n :: N) where
  type NumberC n :: Constraint
  nat :: Nat n
instance KnownNumber Ze where
  type NumberC Ze = (() :: Constraint)
  nat = Z_
instance KnownNumber n => KnownNumber (Su n) where
  type NumberC (Su n) = KnownNumber n
  nat = S_ nat

-- PolyKind Functors {{{

newtype Id a = I
  { runI :: a
  } deriving (Eq,Ord,Show)

instance Functor Id where
  fmap f = I . f . runI

instance Applicative Id where
  pure = I
  I f <*> I a = I $ f a

instance Monad Id where
  I a >>= f = f a

newtype Const r a = C
  { getC :: r
  }

instance Functor (Const r) where
  fmap _ (C r) = C r

instance Monoid r => Applicative (Const r) where
  pure _ = C mempty
  C r <*> C s = C $ r <> s

newtype Comp (f :: l -> *) (g :: k -> l) (a :: k) = Comp
  { getComp :: f (g a)
  }

instance (Functor f, Functor g) => Functor (Comp f g) where
  fmap f = Comp . fmap (fmap f) . getComp

instance (Applicative f, Applicative g) => Applicative (Comp f g) where
  pure  = Comp . pure . pure
  Comp f <*> Comp a = Comp $ (<*>) <$> f <*> a

liftComp :: Applicative f => g a -> Comp f g a
liftComp = Comp . pure

-- }}}

imp :: a
imp = error "impossible type!"

data E :: * -> * where
  E :: UI Element -> E a

type Render f = forall a. f a -> UI Element

renderCells :: Render f -> Rows f as -> Rows E as
renderCells f = map $ map' (E . f)

