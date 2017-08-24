module Test.Main where

import Prelude

import Bundaegi (class HasTSRep, genericToTSRep, getTSRep)
import Bundaegi.IOTS (class HasIOTSRep, genericToIOTSRep, getIOTSRep)
import Control.Monad.Eff.Console (log)
import Data.Generic.Rep (class Generic)
import Data.List (intercalate)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Text.Prettier (defaultOptions, format)
import Type.Prelude (Proxy(..))

data Fruits
  = Watermelon
  | Grapes Number
  | Banana { color :: String, count :: Number }
derive instance genericFruits :: Generic Fruits _
instance fruitsHasTSRep :: HasTSRep Fruits where
  toTSRep = genericToTSRep
instance fruitsHasIOTSRep :: HasIOTSRep Fruits where
  toIOTSRep = genericToIOTSRep

main = do
  log' $ getTSRep "Fruits" (Proxy :: Proxy Fruits)
  log' $ getTSRep "Thingy" (Proxy :: Proxy {a :: Number, b :: Boolean, c :: {d :: String}})
  log' $ getIOTSRep "iFruits" (Proxy :: Proxy Fruits)
  writeTextFile UTF8 "test/generated.ts" contents
  where
    log' = log <<< format defaultOptions
    lines =
      intercalate ";" $
        [ getTSRep "Fruits" (Proxy :: Proxy Fruits)
        , getTSRep "Thingy" (Proxy :: Proxy {a :: Number, b :: Boolean, c :: {d :: String}})
        , getIOTSRep "iFruits" (Proxy :: Proxy Fruits)
        , getIOTSRep "iThingy" (Proxy :: Proxy {a :: Number, b :: Boolean, c :: {d :: String}})
        ]
    contents = format defaultOptions $ "import * as t from 'io-ts';" <> lines

