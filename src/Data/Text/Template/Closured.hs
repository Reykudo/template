{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Text.Template.Closured (
  Template (..),
  Context,
  ContextA,
  Frag,
  renderA,
  render,
  renderLazy,
  substitute,
  substituteA,
  templateConvert,
  templateConvertOptimized,
  template,
  templateSafe,
) where

import Control.DeepSeq (deepseq)
import Control.Monad
import Control.Monad.StrictIdentity (
  StrictIdentity (StrictIdentity),
  runStrictIdentity,
 )
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text hiding (foldl', foldr, foldr')
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Data.Text.Template.Parse qualified as Base
import GHC.List (foldl', foldr')

{- | A representation of a 'Data.Text' template, supporting efficient
rendering.
-}
newtype Template f = Template {unTemplate :: Frag f}

-- | A mapping from placeholders in the template to values.
type Context = T.Text -> TB.Builder

-- | Like 'Context', but with an applicative lookup function.
type ContextA f = T.Text -> f TB.Builder

-- -- | A template fragment.
newtype Frag f = Frag {unFrag :: ContextA f -> f TB.Builder}

instance (Monad f) => Semigroup (Frag f) where
  a <> b =
    let closure !ctx = do
          ares <- a.unFrag ctx
          bres <- b.unFrag ctx
          pure $ (<>) ares bres
     in Frag closure
  {-# INLINE (<>) #-}

instance (Monad f) => Monoid (Frag f) where
  mempty = let closure _ctx = pure "" in Frag closure
  {-# INLINE mempty #-}

{- | Like 'render', but allows the lookup to have side effects.  The
lookups are performed in order that they are needed to generate the
resulting text.

You can use this e.g. to report errors when a lookup cannot be made
successfully.  For example, given a list @ctx@ of key-value pairs
and a 'Template' @tmpl@:

> renderA tmpl (flip lookup ctx)

will return 'Nothing' if any of the placeholders in the template
don't appear in @ctx@ and @Just text@ otherwise.
-}
renderA :: (Monad f) => Template f -> ContextA f -> f TB.Builder
{-# INLINE renderA #-}
renderA (Template frag) = frag.unFrag
{-# SPECIALIZE INLINE renderA :: Template IO -> ContextA IO -> IO TB.Builder #-}

-- | Perform the template substitution, returning a new 'LT.Text'.
render :: Template StrictIdentity -> Context -> TB.Builder
{-# INLINE render #-}
render t ctxFunc = runStrictIdentity $ renderA t (StrictIdentity . ctxFunc)

-- | Perform the template substitution, returning a new 'LT.Text'.
renderLazy :: Template Identity -> Context -> TB.Builder
{-# INLINE renderLazy #-}
renderLazy t ctxFunc = runIdentity $ renderA t (Identity . ctxFunc)

{- | Perform the template substitution, returning a new 'LT.Text'.  A
malformed template string will raise an 'error'.  Note that

> substitute tmpl ctx == render (template tmpl) ctx
-}
substitute :: T.Text -> Context -> TB.Builder
{-# INLINE substitute #-}
substitute = render . template

{- | Perform the template substitution in the given 'Applicative',
returning a new 'LT.Text'. Note that

> substituteA tmpl ctx == renderA (template tmpl) ctx
-}
substituteA :: (Monad f) => T.Text -> ContextA f -> f TB.Builder
{-# INLINE substituteA #-}
substituteA = renderA . template

templateConvertOptimized :: forall f. (Monad f) => Base.Template -> Template f
templateConvertOptimized (Base.Template frags) = Template . foldMap (mLits Map.!) $ frags
 where
  mLits :: Map.Map Base.Frag (Frag f)
  mLits =
    Map.fromList
      $ fmap
        ( \frag ->
            ( frag
            , closureFrag frag
            )
        )
        frags

  closureFrag :: Base.Frag -> Frag f
  closureFrag = \case
    Base.Lit t ->
      let tb = TB.fromText t
          closure _ctx = pure tb
       in Frag closure
    Base.Var t _ ->
      let closure ctx = ctx t
       in Frag closure
{-# INLINEABLE templateConvertOptimized #-}
{-# SPECIALIZE INLINE templateConvertOptimized :: Base.Template -> Template IO #-}

templateConvert :: forall f. (Monad f) => Base.Template -> Template f
templateConvert = Template . foldMap closureFrag . (.unTemplate)
 where
  closureFrag :: Base.Frag -> Frag f
  closureFrag = \case
    Base.Lit t ->
      let tb = TB.fromText t
          closure _ctx = pure tb
       in Frag closure
    Base.Var t _ ->
      let closure ctx = ctx t
       in Frag closure
{-# INLINEABLE templateConvert #-}

template :: (Monad f) => Text -> Template f
template = templateConvert . Base.template
{-# INLINEABLE template #-}

templateSafe :: (Monad f) => T.Text -> Either (Int, Int) (Template f)
templateSafe = fmap templateConvert . Base.templateSafe

-- strict2 :: (a -> b -> c) -> a -> b -> c
-- {-# INLINE strict2 #-}
-- strict2 f !a !b = f a b

-- strict1 :: (a -> b) -> a -> b
-- {-# INLINE strict1 #-}
-- strict1 f !a = f a

-- a =
--   substitute
--     "${var1} and A and ${var2} and ${var3}"
--     ( \case
--         "var1" -> TB.fromText "(Hello from var1)"
--         "var2" -> TB.fromText "(Hello from var2)"
--         _ -> TB.fromText "(unknown)"
--     )