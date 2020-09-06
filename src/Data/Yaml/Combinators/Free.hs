-- | Twan van Laarhoven’s free applicative (see
-- <https://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors>)
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
module Data.Yaml.Combinators.Free where

data Free f a where
  Pure :: a -> Free f a
  Ap :: Free f (a -> b) -> f a -> Free f b

instance Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Ap tx ay) = Ap ((f .) <$> tx) ay

instance Applicative (Free f) where
  pure = Pure
  Pure f <*> tx = fmap f tx
  Ap tx ay <*> tz = Ap (flip <$> tx <*> tz) ay

lift :: f a -> Free f a
lift = Ap (Pure id)

-- | A strict, tail-recursive monoidal foldMap over a free applicative functor
foldMap :: forall a b f . Monoid b => (forall c . f c -> b) -> Free f a -> b
foldMap f free0 = go free0 mempty
  where
    go :: forall c . Free f c -> b -> b
    go free acc = case free of
      Pure _ -> acc
      Ap free' base -> go free' $! f base <> acc

run :: forall a f g . Applicative g => (forall c . f c -> g c) -> Free f a -> g a
run f = go
  where
    go :: forall c . Free f c -> g c
    go free = case free of
      Pure a -> pure a
      Ap free' base -> go free' <*> (f base)
