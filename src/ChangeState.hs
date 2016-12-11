{-# LANGUAGE ScopedTypeVariables #-}

module ChangeState (changeState) where

-- | An easy way to change the type of Parsec user state? The <http://stackoverflow.com/a/17970063
-- solution>.
import           Text.Parsec

changeState :: forall m s u v a. Monad m
                              => (u -> v)
                              -> (v -> u)
                              -> ParsecT s u m a
                              -> ParsecT s v m a
changeState backward forward = mkPT . transform . runParsecT
  where
    mapState :: forall u' v'. (u' -> v') -> State s u' -> State s v'
    mapState f st = st { stateUser = f (stateUser st) }

    mapReply :: forall u' v'. (u' -> v') -> Reply s u' a -> Reply s v' a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e

    fmap3 = fmap . fmap . fmap

    transform :: (State s u -> m (Consumed (m (Reply s u a))))
              -> (State s v -> m (Consumed (m (Reply s v a))))
    transform p st = fmap3 (mapReply backward) (p (mapState forward st))
