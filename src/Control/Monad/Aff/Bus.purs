{-
Copyright 2018 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Control.Monad.Aff.Bus
  ( make
  , read
  , write
  , split
  , kill
  , Cap
  , Bus
  , BusRW
  , BusR
  , BusR'
  , BusW
  , BusW'
  ) where

import Prelude

import Data.Foldable (foldl, sequence_, traverse_)
import Data.List (List, (:))
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.AVar as EffAvar
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Class (class MonadEffect, liftEffect)

data Cap

data Bus (r ∷ # Type) a = Bus (AVar a) (AVar (List (AVar a)))

type BusR = BusR' ()

type BusR' r = Bus (read ∷ Cap | r)

type BusW = BusW' ()

type BusW' r = Bus (write ∷ Cap | r)

type BusRW = Bus (read ∷ Cap, write ∷ Cap)

-- | Creates a new bidirectional Bus which can be read from and written to.
make ∷ ∀ m a. MonadEffect m => m (BusRW a)
make = liftEffect do
  cell ← EffAvar.empty
  consumers ← EffAvar.new mempty
  let
    loop = attempt (AVar.take cell) >>= traverse_ \res → do
      vars ← AVar.take consumers
      AVar.put mempty consumers
      sequence_ (foldl (\xs a → AVar.put res a : xs) mempty vars)
      loop
  launchAff_ loop

  pure $ Bus cell consumers

-- | Blocks until a new value is pushed to the Bus, returning the value.
read ∷ ∀ a r. BusR' r a → Aff a
read (Bus _ consumers) = do
  res' ← AVar.empty
  cs ← AVar.take consumers
  AVar.put (res' : cs) consumers
  AVar.take res'

-- | Pushes a new value to the Bus, yieldig immediately.
write ∷ ∀ a r. a → BusW' r a → Aff Unit
write a (Bus cell _) = AVar.put a cell

-- | Splits a bidirectional Bus into separate read and write Buses.
split ∷ ∀ a. BusRW a → Tuple (BusR a) (BusW a)
split (Bus a b) = Tuple (Bus a b) (Bus a b)

-- | Kills the Bus and propagates the exception to all consumers.
kill ∷ ∀ a r. Aff.Error → BusW' r a → Aff Unit
kill err (Bus cell consumers) = do
  AVar.kill err cell
  vars ← AVar.take consumers
  traverse_ (AVar.kill err) vars
