{-# LANGUAGE RankNTypes, TypeOperators, PatternSignatures #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

import Control.Monad.State
import Prelude hiding (or,and,not)

data (f :+: g) a = Inl (f a) | Inr (g a)
infixr 6 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

data Term = Const String [Term]
          | Var String

data TT a     = TT
data FF a     = FF
data Atom a   = Atom String [Term]
data Not a    = Not a
data Or a     = Or a a
data And a    = And a a
data Impl a   = Impl a a
data Exists a = Exists (Term -> a)
data Forall a = Forall (Term -> a)

class (Functor sub, Functor sup) => sub :->: sup where
  inj :: sub a -> sup a

instance Functor f => (:->:) f f where
  inj = id

instance (Functor f, Functor g) => (:->:) f (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, (f :->: g))
  => (:->:) f (h :+: g) where 
    inj = Inr . inj

type Input = TT :+: FF :+: Atom
            :+: Not :+: Or :+: And :+: Impl
            :+: Exists :+: Forall

data Formula f = In { out :: f (Formula f) }

foldFormula :: Functor f => (f a -> a) -> Formula f -> a
foldFormula algebra = algebra . fmap (foldFormula algebra) . out

--foodFact :: Formula Input
--foodFact = TT 
