module State where

import Control.Arrow

newtype State s v = St {runSt :: s -> (v , s)}

instance Functor (State s) where
  fmap f m = St $ \s' -> first f (runSt m s')

instance Applicative (State s) where
  pure a = St $ \s -> (a, s)
  f <*> x = St $ \s ->
                   let (f' , s' ) = runSt f s
                       (x' , s'' ) = runSt x s'
                   in (f' x' , s'' )

instance Monad (State s) where
  return = pure
  x >>= g = St $ \s ->
                   let (x' , s' ) = runSt x s
                        in runSt (g x') s'

get :: State a a
get = St $ \s -> (s,s)

put :: b -> State b ()
put b = St $ const ((), b)

modify :: (s -> s) -> State s ()
modify f = get >>= (put . f)
