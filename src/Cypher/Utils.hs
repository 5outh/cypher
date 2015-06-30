{-# LANGUAGE OverloadedStrings #-}

module Cypher.Utils where

import Data.Monoid
import qualified Data.Text as T
import Control.Monad.Free

type Endo a = a -> a

liftFn :: (MonadFree f m, Functor f) => ((a -> a) -> f b) -> m b
liftFn f = liftF (f id)

append :: Monoid a => a -> a -> a
append = flip (<>)

ident :: Int -> T.Text
ident x = T.pack $ show x
