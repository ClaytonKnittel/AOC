{-# LANGUAGE FunctionalDependencies #-}

module Lib
  ( test,
  )
where

class Monad m => MonadState2 s m | m -> s where
  get :: m s
  get = state (\s -> (s, s))

  put :: s -> m ()
  put s = state $ const ((), s)

  state :: (s -> (a, s)) -> m a
  state f = do
    s <- get
    let ~(a, s') = f s
    put s'
    return a

test :: IO ()
test = putStrLn "someFunc"
