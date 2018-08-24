{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}

module TigerTraversals where

import Prelude hiding (Traversable)

import           TigerAbs
import           TigerSymbol

import           Data.Functor.Constant
import           Data.Functor.Identity

import           Data.Monoid

class Traversable d t where
  traversable :: Applicative f => (d -> f d) -> (t -> f t)

-- class TraverseExp t where
--   traverseExp :: Applicative f => (Exp -> f Exp) -> (t -> f t)

-- class TraverseVar t where
--   traverseVar :: Applicative f => (Var -> f Var) -> (t -> f t)

instance Traversable Exp Exp where
  traversable _ (VarExp v p) = pure $ VarExp v p
  traversable _ (UnitExp p) = pure $ UnitExp p
  traversable _ (BreakExp p) = pure $ BreakExp p
  traversable _ (NilExp p) = pure $ NilExp p
  traversable _ (IntExp i p) = pure $ IntExp i p
  traversable _ (StringExp str p) = pure $ StringExp str p
  traversable f (CallExp sym xs p) = CallExp sym <$> traverse f xs <*> pure p
  traversable f (OpExp l o r p) = OpExp <$> f l <*> pure o <*> f r <*> pure p
  traversable f (RecordExp rs s p) = RecordExp <$> traverse (traverse f) rs <*> pure s <*> pure p
  traversable f (SeqExp xs p) = SeqExp <$> traverse f xs <*> pure p
  traversable f (AssignExp var exp p) = (AssignExp var) <$> f exp <*> pure p
  traversable f (IfExp cond tt ff p) = IfExp <$> f cond <*> f tt  <*> traverse f ff <*> pure p
  traversable f (WhileExp cond body p) = WhileExp <$> f cond <*> f body <*> pure p
  traversable f (ForExp sym mbbool c b i p) = ForExp sym mbbool <$> f c <*> f b <*> f i <*> pure p
  traversable f (LetExp decs body p) = LetExp <$> traverse (traversable f) decs <*> f body <*> pure p
  traversable f (ArrayExp sym l init p ) = ArrayExp sym <$> f l <*> f init <*> pure p

instance Traversable Exp Var where
  traversable f = traversable (\case
                                  FieldVar v s -> FieldVar <$> traversable f v <*> pure s
                                  SubscriptVar v e -> SubscriptVar <$> traversable f v <*> f e
                                  e -> traversable f e
                              )

instance Traversable Exp Dec where
  traversable f (FunctionDec xs) = FunctionDec <$> traverse (\(sym, fs, mbs, e, p)
                                                              -> (,,,,) <$>
                                                                 pure sym <*>
                                                                 pure fs <*>
                                                                 pure mbs <*>
                                                                 f e <*> pure p) xs
  traversable f (VarDec sym mb mbs init p) = VarDec sym mb mbs <$> f init <*> pure p
  traversable _ (TypeDec ts) = pure $ TypeDec ts

instance Traversable Var Var where
  traversable _ (SimpleVar sym) = pure $ SimpleVar sym
  traversable f (FieldVar v sym) = FieldVar <$> f v <*> pure sym
  traversable f (SubscriptVar v e) = SubscriptVar <$> f v <*> traversable f e

instance Traversable Var Exp where
  traversable f = traversable cas
    where
      cas (VarExp v p) = VarExp <$> f v <*> pure p
      cas (AssignExp v init p) = AssignExp <$> f v <*> traversable f init <*> pure p
      cas e = traversable cas e

instance Traversable Var Dec where
  traversable _ = pure

travMap :: Traversable t t => (t -> t) -> (t -> t)
travMap f = runIdentity . traversable (Identity . f)

travFold :: (Monoid a, Traversable t t) => (t -> a) -> (t -> a)
travFold f = getConstant . traversable (Constant . f)

countPrints :: Exp -> Int
countPrints = getSum . travFold caso
  where
    caso (CallExp s _ _) = if unpack s == "print" then Sum 1
                           else Sum 0
    caso e = travFold caso e
