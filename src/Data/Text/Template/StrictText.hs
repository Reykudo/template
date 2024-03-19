{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Text.Template.StrictText (
  Template (..),
  Context,
  ContextA,
  Frag (..),
  renderA,
  render,
  substitute,
  substituteA,
  module Data.Text.Template.Parse,
) where

import Data.Foldable (Foldable (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Template.Parse (
  Frag (Lit, Var),
  Template (Template),
  showTemplate,
  template,
  templateSafe,
 )

-- | A mapping from placeholders in the template to values.
type Context = T.Text -> T.Text

-- | Like 'Context', but with an applicative lookup function.
type ContextA f = T.Text -> f T.Text

-- | Perform the template substitution, returning a new 'LT.Text'.
render :: Template -> Context -> Text
{-# INLINE render #-}
render (Template frags) ctxFunc = foldMap' renderFrag frags
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
renderA :: (Applicative f) => Template -> ContextA f -> f Text
{-# INLINE renderA #-}
renderA (Template frags) ctxFunc = foldMap' id <$> traverse renderFrag frags
 where
  renderFrag (Lit s) = pure s
  renderFrag (Var x _) = ctxFunc x

{- | Perform the template substitution, returning a new 'LT.Text'.  A
malformed template string will raise an 'error'.  Note that

> substitute tmpl ctx == render (template tmpl) ctx
-}
substitute :: T.Text -> Context -> Text
{-# INLINE substitute #-}
substitute = render . template

{- | Perform the template substitution in the given 'Applicative',
returning a new 'LT.Text'. Note that

> substituteA tmpl ctx == renderA (template tmpl) ctx
-}
substituteA :: (Applicative f) => T.Text -> ContextA f -> f Text
{-# INLINE substituteA #-}
substituteA = renderA . template
