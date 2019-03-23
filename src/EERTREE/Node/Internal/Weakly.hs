{-# LANGUAGE ExistentialQuantification #-}
module EERTREE.Node.Internal.Weakly where

import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.Weak

-- | 'Weakly' stores a weak link to the result of
-- some function application. Weak link means that
-- the result can be garbage collected after its been
-- used even if weak links still exist.
--
-- However, @'Weakly' a@ also contains a recipe of how
-- to constuct that value if it is needed again in the
-- future.
--
-- This makes 'Weakly' useful when working with large
-- lazy graph-like data structures to reduce memory usage
-- when a working part of that structure changes.
data Weakly a = forall b. Weakly
  { wrRecipe     :: b -> a          -- ^ How to (re)construct a value.
  , wrIngredient :: b               -- ^ Source.
  , wrResult     :: IORef (Weak a)  -- ^ A weak link to result.
  }

--  instance Functor Weakly where
--    fmap f = applyWeakly (f . fromWeakly)
--
--  instance Applicative Weakly where
--    pure = applyWeakly id
--    wf <*> wx = applyWeakly (fromWeakly wf . fromWeakly) wx
--
--  instance Monad Weakly where
--    return = pure
--    wx >>= f = applyWeakly (fromWeakly . f . fromWeakly) wx
--
-- instance Comonad Weakly where
--   extract = fromWeakly
--   extend = applyWeakly

-- | Apply function to an argument producing
-- a result that is weakly referenced
-- (and hence can be garbage collected).
applyWeakly :: (a -> b) -> a -> Weakly b
{-# NOINLINE applyWeakly #-}
applyWeakly f x = unsafePerformIO $ do
  let y = f x
  w <- mkWeak y y Nothing
  Weakly f x <$> newIORef w

-- | Extract weakly referenced result,
-- reevaluating if necessary.
fromWeakly :: Weakly b -> b
{-# NOINLINE fromWeakly #-}
fromWeakly (Weakly f x wref) = unsafePerformIO $ do
  w  <- readIORef wref
  mv <- deRefWeak w
  case mv of
    Just v  -> return v
    Nothing -> do
      let y = f x
      w' <- mkWeak y y Nothing
      writeIORef wref w'
      return y

instance Show a => Show (Weakly a) where
  show = show . fromWeakly

