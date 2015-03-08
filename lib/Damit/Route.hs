{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- |
--
-- Unless you have only a shortcut or two—in which case I don't think damit is very
-- useful to you—you want to organize them in some fashion.  This module provides an
-- organizational framework that should be both reasonably convenient for you to use
-- and simple enough for damit to understand.
--
-- The damit's routing is modelled after some web frameworks, in particular Scotty and
-- Sinatra, but with fewer regular expressions and more composability.  The basic building
-- block is the thing called 'Pattern'.  The simplest patterns compare a single piece
-- of input with something or bind it to a variable.  Patterns, being 'Applicative', can be
-- composed using helpful combinators from the "Control.Applicative" module.
--
-- Once you compose yourself a 'Pattern', you feed it to 'when' (or 'when_') to obtain a
-- 'RouteT'. Since you probably have a lot of patterns, you'll end up with a lot of
-- 'RouteT's! Don't worry, it's easy to compose them too: 'RouteT' is a monad transformer
-- in the style of the "transformers" package.
--
-- Finally, to get the routed value (or lack thereof) given some input and a 'RouteT', put
-- them into 'route'.
module Damit.Route
  ( RouteT
  , Route
  , route
  , Input
  , with
  , with_
  , Pattern
  , (Damit.Route.**)
  , be
  , satisfy
  , fit
  , bind
  , pop
  , eop
  , Arg(..)
  , inputs
  ) where

import Control.Applicative (Applicative(..), Alternative(empty, (<|>)), (<$), liftA2)
import Control.Monad (MonadPlus(..), liftM, liftM2, guard)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.String (IsString(..))
import Data.Traversable (traverse)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Text.Read (readMaybe)

infixl 4 ** -- same fixity as '(<*>)', '(<*)', and '(*>)'


-- | Monad transformer that threads the input through the patterns.
newtype RouteT s m a = RouteT
  { unRouteT :: Input s -> m (Maybe a)
  } deriving (Functor)

-- | An alias for 'RouteT' without any underlying effects.
type Route s = RouteT s Identity

-- | Inputs for routing.
type Input s = [s]

instance Applicative m => Applicative (RouteT s m) where
  pure = RouteT . pure . pure . pure
  RouteT f <*> RouteT x =
    RouteT ((liftA2.liftA2) (<*>) f x)

instance Applicative m => Alternative (RouteT s m) where
  empty = RouteT (\_ -> pure empty)
  RouteT x <|> RouteT y =
    RouteT ((liftA2.liftA2) (<|>) x y)

instance Monad m => Monad (RouteT s m) where
  return = RouteT . return . return . return
  RouteT mx >>= k =
    RouteT (\i -> do
      x <- mx i
      case x of
        Just y  -> unRouteT (k y) i
        Nothing -> return Nothing)

instance Monad m => MonadPlus (RouteT s m) where
  mzero = RouteT (\_ -> return mzero)
  RouteT x `mplus` RouteT y =
    RouteT ((liftM2.liftM2) mplus x y)

instance MonadIO m => MonadIO (RouteT s m) where
  liftIO = lift . liftIO

instance MonadTrans (RouteT s) where
  lift = RouteT . const . liftM Just

-- | "Run"-like function for 'RouteT'. Give it an input, and watch the
-- routed value appear, if any.
route :: Input s -> RouteT s m a -> m (Maybe a)
route = flip unRouteT

-- | @with pat routes@ only routes through routes if the input matches the pattern.
with :: Applicative m => Pattern s a -> (a -> m b) -> RouteT s m b
with p f = RouteT (traverse f . match p)

-- | A 'with' variant, useful when you don't care about the bound values.
with_ :: Applicative m => Pattern s a -> m b -> RouteT s m b
with_ p f = with p (\_ -> f)

match :: Pattern s a -> Input s -> Maybe a
match p = fmap snd . unPattern (p <* eop)

-- | 'Pattern's match pieces of input
newtype Pattern s a = Pattern
  { unPattern :: Input s -> Maybe (Input s, a)
  } deriving (Functor)

instance Applicative (Pattern s) where
  pure x = Pattern (\i -> Just (i, x))
  Pattern f <*> Pattern x = Pattern $ \i -> do
    (i',  ab) <- f i
    (i'', a)  <- x i'
    return (i'', ab a)

instance Alternative (Pattern s) where
  empty = Pattern (\_ -> empty)
  Pattern x <|> Pattern y = Pattern (liftA2 (<|>) x y)

instance Monad (Pattern s) where
  return x = Pattern (\i -> Just (i, x))
  Pattern x >>= k = Pattern $ \i -> do
    (i',  a) <- x i
    (i'', b) <- unPattern (k a) i'
    return (i'', b)

instance MonadPlus (Pattern s) where
  mzero = Pattern (\_ -> mzero)
  Pattern x `mplus` Pattern y = Pattern (liftM2 mplus x y)

instance (Eq s, IsString s, s ~ a) => IsString (Pattern s a) where
  fromString = be . fromString

-- | This operation is from the alternative formulation of 'Applicative'. It clashes
-- with "Prelude"'s @(**)@ but everyone knows "Prelude" is silly, right?
(**) :: Applicative f => f a -> f b -> f (a, b)
(**) = liftA2 (,)

-- | @be "foo"@ checks the next input piece is \"foo\". If that's the case
-- that \"foo\" is then returned.
be :: Eq s => s -> Pattern s s
be s = satisfy (== s)

-- | @satisfy p@ checks the next input piece satisfies the predicate @p@. If that's
-- the case that input piece is then returned.
satisfy :: (s -> Bool) -> Pattern s s
satisfy p = fit (\s -> s <$ guard (p s))

-- | @fit f@ checks the next input piece fits the function @f@. Fitting
-- means applying @f@ to the piece results in @Just _@. If that's the case
-- that input piece is then returned.
fit :: (s -> Maybe t) -> Pattern s t
fit f = do i <- pop; maybe empty pure (f i)

-- | @bind@ parses the next input piece using 'parse'.
bind :: Arg s a => Pattern s a
bind = pat Nothing (\x xs -> fmap (\y -> (xs, y)) (parse x))

-- | Pop the next piece of input; fail if there's nothing to pop.
pop :: Pattern s s
pop = pat Nothing (\x xs -> Just (xs, x))

-- | End of pattern; this pattern fails if unparsed input is still there.
eop :: Pattern s ()
eop = pat (Just ([], ())) (\_ _ -> Nothing)

pat :: Maybe (Input s, a) -> (s -> [s] -> Maybe (Input s, a)) -> Pattern s a
pat z f = Pattern (\i -> case i of [] -> z; x : xs -> f x xs)

-- | How to get from a 'String'-like @s@ to an @a@?
--
-- In the real world the @s@ is always 'String'. mostly because the
-- related libraries, such as "process" and "directory", use 'String's for
-- everything. It's easy to imagine @s@ being any 'String'-like type
-- like 'Text' though.
class Arg s a where
  parse :: s -> Maybe a

instance Arg String String where parse = Just
instance Arg String Integer where parse = readMaybe
instance Arg String Int where parse = readMaybe
instance Arg String Int64 where parse = readMaybe
instance Arg String Int32 where parse = readMaybe
instance Arg String Int16 where parse = readMaybe
instance Arg String Int8 where parse = readMaybe
instance Arg String Word where parse = readMaybe
instance Arg String Word64 where parse = readMaybe
instance Arg String Word32 where parse = readMaybe
instance Arg String Word16 where parse = readMaybe
instance Arg String Word8 where parse = readMaybe
instance Arg String Double where parse = readMaybe
instance Arg String Float where parse = readMaybe

-- | Retrieve the inputs. Because we can.
inputs :: Monad m => RouteT s m (Input s)
inputs = RouteT (return . Just)
