module Main where

import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Data.Text.IO qualified as T
import Data.Text.Template (Context, substitute)

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = TB.fromText $ maybe err id . lookup x $ assocs
 where
  err = error $ "Could not find key: " ++ T.unpack x

main :: IO ()
main = T.putStr $ substitute helloTemplate helloContext
 where
  helloTemplate = T.pack "Hello, ${AUCTION_ID}!\n"
  helloContext = context [("AUCTION_ID", "Joe")]
