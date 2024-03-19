{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Text.Template.Builder (
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
  showTemplate,
  templateSafe,
) where

import Data.Foldable (Foldable (..))
import Data.Text
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Template.Parse qualified as Base

{- | A representation of a 'Data.Text' template, supporting efficient
rendering.
-}
newtype Template = Template {unTemplate :: [Frag]}

-- | A mapping from placeholders in the template to values.
type Context = T.Text -> TB.Builder

-- | Like 'Context', but with an applicative lookup function.
type ContextA f = T.Text -> f TB.Builder

-- | A template fragment.
data Frag = Lit !TB.Builder | Var !T.Text !Bool

-- | Perform the template substitution, returning a new 'LT.Text'.
render :: Template -> Context -> TB.Builder
{-# INLINE render #-}
render (Template frags) ctxFunc = foldMap' id $ fmap renderFrag frags
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
renderA :: (Applicative f) => Template -> ContextA f -> f TB.Builder
{-# INLINE renderA #-}
renderA (Template frags) ctxFunc = foldMap' id <$> traverse renderFrag frags
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
substituteA :: (Applicative f) => T.Text -> ContextA f -> f TB.Builder
{-# INLINE substituteA #-}
substituteA = renderA . template

template :: Text -> Template
template = templateConvert . Base.template
{-# INLINEABLE template #-}

templateSafe :: T.Text -> Either (Int, Int) Template
templateSafe = fmap templateConvert . Base.templateSafe

-- | Show the template string.
showTemplate :: Template -> T.Text
showTemplate = Base.showTemplate . templateConvertReverse

templateConvert :: Base.Template -> Template
templateConvert =
  Template
    . fmap
      ( \case
          Base.Lit t -> Lit (TB.fromText t)
          Base.Var t b -> Var t b
      )
    . (.unTemplate)
{-# INLINEABLE templateConvert #-}

templateConvertReverse :: Template -> Base.Template
templateConvertReverse =
  Base.Template
    . fmap
      ( \case
          Lit t -> Base.Lit $ toStrict $ TB.toLazyText t
          Var t b -> Base.Var t b
      )
    . (.unTemplate)
{-# INLINEABLE templateConvertReverse #-}
