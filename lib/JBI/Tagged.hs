{-# LANGUAGE DefaultSignatures, FlexibleContexts, KindSignatures,
             MultiParamTypeClasses #-}

{- |
   Module      : JBI.Commands.Tagged
   Description : Support for the Tagged type
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module JBI.Tagged
  ( WithTagged (..)
  , stripTag
  , stripTags
  , tag
    -- * Re-export
  , Tagged (..)
  , proxy
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Tagged

--------------------------------------------------------------------------------

class WithTagged (g :: * -> *) where

  -- | Strip off type safety, run the function, put type safety back on.
  withTaggedF :: (Coercible a a', Coercible b b', Functor f)
                 => (a' -> f (g b')) -> Tagged t a -> f (g (Tagged t b))
  default withTaggedF :: ( Coercible a a', Coercible b b', Functor f
                         , Coercible (g b') (g (Tagged t b))
                         , Coercible (g (Tagged t b)) (g b'))
                         => (a' -> f (g b')) -> Tagged t a -> f (g (Tagged t b))
  withTaggedF f = fmap coerce . f . coerce

  tagInner :: Tagged t (g a) -> g (Tagged t a)
  default tagInner :: ( Coercible (Tagged t (g a)) (g (Tagged t a))
                      , Coercible (g (Tagged t a)) (g a))
                      => Tagged t (g a) -> g (Tagged t a)
  tagInner = coerce

  tagOuter :: g (Tagged t a) -> Tagged t (g a)
  default tagOuter :: (Coercible (g (Tagged t a)) (Tagged t (g a)))
                      => g (Tagged t a) -> Tagged t (g a)
  tagOuter = coerce

instance WithTagged Maybe
instance WithTagged []

-- | Remove the tag along with (potentially) any newtype wrappers
--   added on.
stripTag :: (Coercible a a') => Tagged t a -> a'
stripTag = coerce

stripTags :: (Coercible a a') => [Tagged t a] -> [a']
stripTags = coerce

-- | Put the appropriate tag on.
tag :: (Coercible a a') => a -> Tagged t a'
tag = coerce
