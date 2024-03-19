{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Text.Template.Parse (
  -- * The @Template@ type
  Template (..),
  Frag (..),

  -- * Basic interface
  template,
  templateSafe,
  showTemplate,
  cacheLocalityOptimise
)
where

import Control.Monad (
  liftM2,
  replicateM_, (<$!>),
 )
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Char (isAlphaNum)
import Data.Foldable (Foldable (foldMap'))
import Data.Function (on)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Language.Haskell.TH.Lift (deriveLift)
import Prelude hiding (takeWhile)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
-- -----------------------------------------------------------------------------

-- | A template fragment.
data Frag = Lit !T.Text | Var !T.Text !Bool
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NFData


deriveLift ''Frag

{- | A representation of a 'Data.Text' template, supporting efficient
rendering.
-}
newtype Template = Template {unTemplate :: [Frag]}

deriveLift ''Template


cacheLocalityOptimise :: Template -> Template
cacheLocalityOptimise (Template frags) = Template $! (m Map.!) <$!> frags
  where m = Map.fromList [ (frag, frag) | frag <- frags ]

instance Eq Template where
  (==) = (==) `on` showTemplate

append :: Template -> Template -> Template
Template frags `append` Template frags' = Template $ frags ++ frags'

{- | Property that holds:

@
template x <> template y = template $ x \`T.append\` y
@
-}
instance Semigroup Template where
  (<>) = append

{- | Properties that hold:

1. @template \"\" = mempty@

2. @template x \`mappend\` template y = template $ x \`T.append\` y@
-}
instance Monoid Template where
  mempty = Template []

instance Show Template where
  show = T.unpack . showTemplate

-- | Show the template string.
showTemplate :: Template -> T.Text
showTemplate (Template fs) = T.concat $ map showFrag fs

instance Show Frag where
  show = T.unpack . showFrag

showFrag :: Frag -> T.Text
showFrag (Var s b)
  | b = T.concat [T.pack "${", s, T.pack "}"]
  | otherwise = T.concat [T.pack "$", s]
showFrag (Lit s) = T.pack $ concatMap escape $ show s
 where
  escape '$' = "$$"
  escape c = [c]

-- -----------------------------------------------------------------------------
-- Basic interface

{- | Create a template from a template string.  A malformed template
string will raise an 'error'.
-}
template :: T.Text -> Template
template = templateFromFrags . runParser pFrags
{-# INLINEABLE template #-}

{- | Create a template from a template string.  A malformed template
string will cause 'templateSafe' to return @Left (row, col)@, where
@row@ starts at 1 and @col@ at 0.
-}
templateSafe :: T.Text -> Either (Int, Int) Template
templateSafe =
  fmap templateFromFrags . runParser pFragsSafe

templateFromFrags :: [Frag] -> Template
templateFromFrags = Template . combineLits

combineLits :: [Frag] -> [Frag]
combineLits [] = []
combineLits xs =
  let (lits, xs') = span isLit xs
   in case lits of
        [] -> gatherVars xs'
        [lit] -> lit : gatherVars xs'
        _ -> Lit (foldMap' id (map fromLit lits)) : gatherVars xs'
 where
  gatherVars [] = []
  gatherVars ys =
    let (vars, ys') = span isVar ys
     in vars ++ combineLits ys'

  isLit (Lit _) = True
  isLit _ = False

  isVar = not . isLit

  fromLit (Lit v) = v
  fromLit _ = undefined

-- Template parser

pFrags :: Parser [Frag]
pFrags = do
  c <- peek
  case c of
    Nothing -> return []
    Just '$' -> continue pVar
    _ -> continue pLit
 where
  continue x = liftM2 (:) x pFrags

pFragsSafe :: Parser (Either (Int, Int) [Frag])
pFragsSafe = pFragsSafe' []
 where
  pFragsSafe' frags = do
    c <- peek
    case c of
      Nothing -> return . Right . reverse $ frags
      Just '$' -> do
        c' <- peekSnd
        case c' of
          Just '$' -> do
            discard 2
            continue (Lit $ T.singleton '$')
          _ -> do
            e <- pVarSafe
            either abort continue e
      _ -> do
        l <- pLit
        continue l
   where
    continue x = pFragsSafe' (x : frags)
    abort = return . Left

pVar :: Parser Frag
pVar = do
  discard 1
  c <- peek
  case c of
    Just '{' -> do
      discard 1
      v <- pIdentifier
      c' <- peek
      case c' of
        Just '}' -> do
          discard 1
          return $ Var v True
        _ -> fmap parseError pos
    _ -> fmap parseError pos

pVarSafe :: Parser (Either (Int, Int) Frag)
pVarSafe = do
  discard 1
  c <- peek
  case c of
    Just '{' -> do
      discard 1
      e <- pIdentifierSafe
      case e of
        Right v -> do
          c' <- peek
          case c' of
            Just '}' -> do
              discard 1
              return $ Right (Var v True)
            _ -> fmap parseErrorSafe pos
        Left m -> return $ Left m
    _ -> fmap parseErrorSafe pos

pIdentifier :: Parser T.Text
pIdentifier = do
  m <- peek
  if isJust m
    then takeWhile isIdentifier1
    else fmap parseError pos

pIdentifierSafe :: Parser (Either (Int, Int) T.Text)
pIdentifierSafe = do
  m <- peek
  if isJust m
    then fmap Right (takeWhile isIdentifier1)
    else fmap parseErrorSafe pos

pLit :: Parser Frag
pLit = do
  s <- takeWhile (/= '$')
  return $ Lit s

isIdentifier1 :: Char -> Bool
isIdentifier1 c = isAlphaNum c || c `elem` "_'"

parseError :: (Int, Int) -> a
parseError = error . makeParseErrorMessage

parseErrorSafe :: (Int, Int) -> Either (Int, Int) a
parseErrorSafe = Left

makeParseErrorMessage :: (Int, Int) -> String
makeParseErrorMessage (row, col) =
  "Invalid placeholder at "
    ++ "row "
    ++ show row
    ++ ", col "
    ++ show col

-- -----------------------------------------------------------------------------
-- Text parser

-- | The parser state.
data S
  = S
      {-# UNPACK #-} !T.Text -- Remaining input
      {-# UNPACK #-} !Int -- Row
      {-# UNPACK #-} !Int -- Col

type Parser = State S

char :: Parser (Maybe Char)
char = do
  S s row col <- get
  if T.null s
    then return Nothing
    else do
      c <- return $! T.head s
      case c of
        '\n' -> put $! S (T.tail s) (row + 1) 1
        _ -> put $! S (T.tail s) row (col + 1)
      return $ Just c

peek :: Parser (Maybe Char)
peek = do
  s <- get
  c <- char
  put s
  return c

peekSnd :: Parser (Maybe Char)
peekSnd = do
  s <- get
  _ <- char
  c <- char
  put s
  return c

takeWhile :: (Char -> Bool) -> Parser T.Text
takeWhile p = do
  S s row col <- get

  case T.span p s of
    (x, s') -> do
      let xlines = T.lines x
          row' = row + fromIntegral (length xlines - 1)
          col' = case xlines of
            [] -> col -- Empty selection
            [sameLine] -> T.length sameLine
            -- Taken from this line
            _ -> T.length (last xlines)
      -- Selection extends
      -- to next line at least
      put $! S s' row' col'
      return x

discard :: Int -> Parser ()
discard n = replicateM_ n char

pos :: Parser (Int, Int)
pos = do
  S _ row col <- get
  return (row, col)

runParser :: Parser a -> T.Text -> a
runParser p s = evalState p $ S s 1 0

-- -----------------------------------------------------------------------------
-- Example

{- $example

Here is an example of a simple substitution:

> {\-# LANGUAGE OverloadedStrings #-\}
>
> module Main where
>
> import qualified Data.ByteString.Lazy as S
> import qualified Data.Text as T
> import qualified Data.Text.Lazy.Encoding as E
>
> import Data.Text.Template
>
> -- | Create 'Context' from association list.
> context :: [(T.Text, T.Text)] -> Context
> context assocs x = maybe err id . lookup x $ assocs
>   where err = error $ "Could not find key: " ++ T.unpack x
>
> main :: IO ()
> main = S.putStr $ E.encodeUtf8 $ substitute helloTemplate helloContext
>   where
>     helloTemplate = "Hello, $name!\n"
>     helloContext  = context [("name", "Joe")]
-}
