module Grain.Render
  ( Render
  , Query
  , runRender
  , useGlobalValue
  , useGlobalUpdater
  , useLocalState
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Grain.Class (class Grain, GProxy)

-- | The type of component renderer.
-- |
-- | In this monad, users can declare that they use some states and updaters.
newtype Render a = Render (ReaderT QueryBox Effect a)

-- Do not derive MonadEffect
derive newtype instance functorRender :: Functor Render
derive newtype instance applyRender :: Apply Render
derive newtype instance applicativeRender :: Applicative Render
derive newtype instance bindRender :: Bind Render
derive newtype instance monadRender :: Monad Render
derive newtype instance semigroupRender :: Semigroup a => Semigroup (Render a)
derive newtype instance monoidRender :: Monoid a => Monoid (Render a)
derive newtype instance monadRecRender :: MonadRec Render

newtype QueryBox = QueryBox Query

type Query =
  { selectGlobalValue :: forall a. Grain a => GProxy a -> Effect a
  , updateGlobalValue :: forall a. Grain a => GProxy a -> (a -> a) -> Effect Unit
  , selectLocalValue :: forall a. Grain a => GProxy a -> Effect a
  , updateLocalValue :: forall a. Grain a => GProxy a -> (a -> a) -> Effect Unit
  }

runRender :: forall a. Render a -> Query -> Effect a
runRender (Render reader) = runReaderT reader <<< QueryBox

-- | Listen a state of received key, then return a state.
-- |
-- | If the received state is changed, the component will be rerendered.
useGlobalValue
  :: forall a
   . Grain a
  => GProxy a
  -> Render a
useGlobalValue proxy = Render do
  QueryBox query <- ask
  withReaderT (const query)
    $ liftEffect $ query.selectGlobalValue proxy

-- | Get an updater of received key.
useGlobalUpdater
  :: forall a
   . Grain a
  => GProxy a
  -> Render ((a -> a) -> Effect Unit)
useGlobalUpdater proxy = Render do
  QueryBox query <- ask
  pure $ query.updateGlobalValue proxy

-- | It is almost same as `useGlobalValue` and `useGlobalUpdater`.
-- |
-- | The difference from them is that the state is treated as component local state.
useLocalState
  :: forall a
   . Grain a
  => GProxy a
  -> Render (Tuple a ((a -> a) -> Effect Unit))
useLocalState proxy = Render do
  QueryBox query <- ask
  withReaderT (const query) do
    value <- liftEffect $ query.selectLocalValue proxy
    pure $ Tuple value $ query.updateLocalValue proxy
