{-#LANGUAGE ScopedTypeVariables, FunctionalDependencies, TypeFamilies, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, AllowAmbiguousTypes, GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators, ViewPatterns, PatternSynonyms, RankNTypes, FlexibleContexts #-}

module Carnap.Core.Unification.Unification (
   Equation((:=:)), UError(..), FirstOrder(..), HigherOrder(..),
      applySub, mapAll, freeVars, emap, sameTypeEq, ExtApp(..), ExtLam(..), 
      EveryPig(..),AnyPig(..), EtaExpand(..), MonadVar(..), betaReduce, 
      betaNormalize, toBNF, pureBNF, toLNF, etaMaximize, etaMaximize'
) where

import Data.Type.Equality
import Data.Typeable
import Carnap.Core.Data.Classes
import Carnap.Core.Util
import Control.Monad.State 

data Equation f where
    (:=:) :: (Typeable a, EtaExpand f a) => f a -> f a -> Equation f

newtype EveryPig f = EveryPig {unEveryPig :: forall a. (Typeable a) => f a}
                     --
--the typeable constraint lets us unpack this in a safe way
data AnyPig f where
    AnyPig :: (EtaExpand f a, Typeable a) => f a -> AnyPig f

instance (UniformlyEq f, UniformlyOrd f) => Ord (AnyPig f) where
    (AnyPig x) <= (AnyPig y) = x <=* y

instance UniformlyEq f => Eq (AnyPig f) where
    (AnyPig x) == (AnyPig y) = x =* y

mutatePig :: (forall a. f a -> f a) -> EveryPig f -> EveryPig f
mutatePig f x = EveryPig (f (unEveryPig x))

instance Schematizable f => Show (Equation f) where
        show (x :=: y) = schematize x [] ++ " :=: " ++ schematize y []

instance UniformlyEq f => Eq (Equation f) where
        (x :=: y) == (x' :=: y') = x =* x' && y =* y'

instance (UniformlyEq f, UniformlyOrd f) => Ord (Equation f) where
        (x :=: y) <= (x' :=: y') =  (x :=: y) == (x' :=: y') 
                                 || (x =* x') && (y <=* y')
                                 || (x <=* x')

--this interface seems simpler for the user to implement than our previous
--1. There is no more varible type
--2. There is no substitution type
--3. other than decompose the operations are simpler. For instance rather than
--   freeVars there is occurs. rather than a full substitution there is just
--   a single varible substitution. rather than combining sameHead and decompose
--   they are seperate methods. rather than converting a varible to check if it
--   it is a varible we just have 'isVar'
--4. Additionally I have tried to allow this to meet the demands of more
--   unification algorithms so that this is a one stop shop for unification
--5. I have tried to name things here in a way that someone reading the HoAR
--   would recognize (hence "decompose" rather than "match")
class UniformlyEq f => FirstOrder f where
    isVar :: f a -> Bool
    sameHead :: f a -> f a -> Bool
    decompose :: f a -> f a -> [Equation f]
    occurs :: f a -> f b -> Bool
    subst :: f a -> f a -> f b -> f b

class Monad m => MonadVar f m where
    fresh :: (Typeable a) => m (f a)
    freshPig :: m (EveryPig f)

class (Typeable a) => EtaExpand f a where
        etaExpand :: f a -> f a
        etaExpand = id

data ExtApp f a where
    ExtApp :: (Typeable b, EtaExpand f b) => f (b -> a) -> f b -> ExtApp f a

data ExtLam f a where
    ExtLam :: (Typeable b, Typeable c, EtaExpand f c, EtaExpand f b) => 
        (f b -> f c) -> (a :~: (b -> c)) -> ExtLam f a

class FirstOrder f => HigherOrder f where
    matchApp :: f a -> Maybe (ExtApp f a)
    castLam ::  f a -> Maybe (ExtLam f a)
    --getLamVar :: f (a -> b) -> f a
    (.$.) :: (Typeable a, Typeable b) => f (a -> b) -> f a -> f b
    lam :: (Typeable a, Typeable b) => (f a -> f b) -> f (a -> b) 

instance {-# OVERLAPPABLE #-} (Typeable a, HigherOrder f) => EtaExpand f a

instance (Typeable b, EtaExpand f b, EtaExpand f a, HigherOrder f) 
        => EtaExpand f (b -> a) where
        etaExpand l  = case castLam l of 
                        Just (ExtLam f Refl) -> lam $ etaExpand . f 
                        Nothing -> lam $ \x -> l .$. x

data UError f where
    SubError :: f a -> f a -> UError f -> UError f
    MatchError ::  f a -> f a -> UError f
    OccursError :: f a -> f a -> UError f

instance Schematizable f => Show (UError f) where
        show (SubError x y e) =  show e ++ "with suberror"
                                 ++ schematize x [] ++ ", "
                                 ++ schematize y []
        show (MatchError x y) = "Match Error:"
                                 ++ schematize x [] ++ ", "
                                 ++ schematize y []
        show (OccursError x y) = "OccursError: "
                                 ++ schematize x [] ++ ", "
                                 ++ schematize y []

sameTypeEq :: Equation f -> Equation f -> Bool
sameTypeEq ((a :: f a) :=: _) ((b :: f b) :=: _) = 
        case eqT :: Maybe (a :~: b) of
            Just Refl -> True
            Nothing -> False

emap :: (forall a. f a -> f a) -> Equation f -> Equation f
emap f (x :=: y) = f x :=: f y

mapAll :: (forall a. f a -> f a) -> [Equation f] -> [Equation f]
mapAll f = map (emap f)

(Left x) .<. f = Left (f x)
x .<. _ = x

applySub :: FirstOrder f => [Equation f] -> f a -> f a
applySub []             y = y
applySub ((v :=: x):ss) y = applySub ss (subst v x y)

freeVars :: (Typeable a, FirstOrder f, EtaExpand f a) => f a -> [AnyPig f]
freeVars t | isVar t   = [AnyPig t]
           | otherwise = concatMap rec (decompose t t)
    where rec (a :=: _) = freeVars a

--------------------------------------------------------
--Beta/Eta operations
--------------------------------------------------------

--return "Nothing" in the do nothing case
betaReduce :: (HigherOrder f) => f a -> Maybe (f a)
betaReduce x = do (ExtApp h t) <- (matchApp x)
                  (ExtLam l Refl) <- (castLam h)
                  return (l t)

--return "Nothing" in the do nothing case
betaNormalize :: (HigherOrder f, MonadVar f m, Typeable a) => f a -> m (Maybe (f a))
betaNormalize x = case (castLam x) of
                     Just (ExtLam f Refl) -> 
                        do v <- fresh
                           inf <- betaNormalize (f v)
                           case inf of
                               Nothing -> return Nothing
                               Just inf' -> return $ Just (lam $ \x -> subst v x inf')
                     Nothing -> case (matchApp x) of
                        Just (ExtApp h t) -> do
                            mh <- betaNormalize h
                            mt <- betaNormalize t
                            case (mh,mt) of
                                (Just h', Just t') -> mbetaNF (h' .$. t') 
                                (Nothing, Just t') -> mbetaNF (h .$. t') 
                                (Just h', Nothing) -> mbetaNF (h' .$. t)
                                (Nothing, Nothing) -> 
                                    case betaReduce x of
                                        Nothing -> return Nothing
                                        Just x' -> mbetaNF x' 
                        Nothing -> return Nothing
        where mbetaNF x = do y <- toBNF x
                             return (Just y)

toBNF :: (HigherOrder f, MonadVar f m, Typeable a) => f a -> m (f a)
toBNF x = do nf <- betaNormalize x
             case nf of
                   Nothing -> return x
                   (Just y) -> return y

pureBNF :: (HigherOrder f, MonadVar f (State Int), Typeable a) => f a -> f a
pureBNF x = evalState (toBNF x) (0 :: Int)

toLNF :: (HigherOrder f, MonadVar f (State Int), Typeable a, EtaExpand f a) => f a -> State Int (f a)
toLNF x = do bnf <- toBNF x
             bnfeta <- etaMaximize bnf
             rec bnfeta
    where rec :: (HigherOrder f, MonadVar f (State Int), Typeable a, EtaExpand f a) => f a -> State Int (f a)
          rec bnfeta = case matchApp bnfeta of 
                          Just (ExtApp h t) -> do t' <- toLNF t
                                                  h' <- case matchApp h of
                                                              Just (ExtApp _ _) -> rec h
                                                              _ -> return h
                                                  return (h' .$. t')
                          Nothing -> case (castLam bnfeta) of
                                   Just (ExtLam f Refl) -> 
                                      do v <- fresh
                                         inf <- rec (f v)
                                         return $ (lam $ \y -> subst v y inf)
                                   Nothing -> return bnfeta

etaMaximize :: (HigherOrder f, Typeable a, EtaExpand f a) => f a -> (State Int) (f a)
etaMaximize = return . etaMaximize'

etaMaximize' :: (HigherOrder f, Typeable a, EtaExpand f a) => f a -> f a
etaMaximize' x = if y =* x then x else etaMaximize' y
    where y = etaExpand x
