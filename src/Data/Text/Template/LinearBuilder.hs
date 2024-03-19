{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- {-# LANGUAGE Strict #-}

module Data.Text.Template.LinearBuilder (
  Template (..),
  Context,
  ContextA,
  Frag (..),
  renderA,
  render,
  substitute,
  substituteA,
  templateConvert,
  template,
  templateConvertOptimized,
  showTemplate,
  templateSafe
) where

import Control.Monad ((<$!>))
import Data.Foldable (Foldable (..))
import Data.Map.Strict qualified as Map
import Data.Text
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Data.Text.Template.Parse qualified as Base
import Data.Vector qualified as V

{- | A representation of a 'Data.Text' template, supporting efficient
rendering.
-}
newtype Template = Template {unTemplate :: V.Vector Frag}

-- data FragList = FragPrepend !Frag !Frags | FragStop

-- | A mapping from placeholders in the template to values.
type Context = T.Text -> TB.Builder

-- | Like 'Context', but with an applicative lookup function.
type ContextA f = T.Text -> f TB.Builder

-- | A template fragment.
data Frag = Lit {-# UNPACK #-} !TB.Builder | Var {-# UNPACK #-} !T.Text {-# UNPACK #-} !Bool

-- | Perform the template substitution, returning a new 'LT.Text'.
render :: Template -> Context -> TB.Builder
{-# INLINE render #-}
render (Template frags) ctxFunc = V.foldr' (flip $ \prev -> (<> prev) . renderFrag) "" frags
 where
  renderFrag (Lit s) = s
  renderFrag (Var x _) = ctxFunc x

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
renderA :: (Monad f) => Template -> ContextA f -> f TB.Builder
{-# INLINE renderA #-}
renderA (Template frags) ctxFunc = V.foldM' (\prev -> fmap (prev <>) . renderFrag) "" frags
 where
  renderFrag (Lit s) = pure s
  renderFrag (Var x _) = ctxFunc x

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

template :: Text -> Template
template = templateConvert . Base.template
{-# INLINEABLE template #-}

templateSafe :: T.Text -> Either (Int, Int) Template
templateSafe = fmap templateConvert . Base.templateSafe

templateConvertOptimized :: Base.Template -> Template
templateConvertOptimized (Base.Template frags) =
  Template
    . V.fromList
    $ (m Map.!)
    <$!> frags
 where
  m =
    Map.fromListWith
      const
      [ ( frag
        , case frag of
            Base.Lit t -> Lit (TB.fromText t)
            Base.Var t b -> Var t b
        )
      | frag <- frags
      ]
{-# INLINEABLE templateConvertOptimized #-}

templateConvert :: Base.Template -> Template
templateConvert =
  Template
    . V.fromList
    . fmap
      ( \case
          Base.Lit t -> Lit (TB.fromText t)
          Base.Var t b -> Var t b
      )
    . (.unTemplate)
{-# INLINEABLE templateConvert #-}

-- strict2 :: (a -> b -> c) -> a -> b -> c
-- {-# INLINE strict2 #-}
-- strict2 f !a !b = f a b

-- strict1 :: (a -> b) -> a -> b
-- {-# INLINE strict1 #-}
-- strict1 f !a = f a
templateConvertReverse :: Template -> Base.Template
templateConvertReverse =
  Base.Template
    . fmap
      ( \case
          Lit t -> Base.Lit $ TB.runBuilder t
          Var t b -> Base.Var t b
      )
    . V.toList
    . (.unTemplate)
{-# INLINEABLE templateConvertReverse #-}

-- | Show the template string.
showTemplate :: Template -> T.Text
showTemplate = Base.showTemplate . templateConvertReverse
