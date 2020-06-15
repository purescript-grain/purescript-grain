module Grain.Render
  ( Render
  , Query
  , runRender
  , useValue
  , useUpdater
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Effect (Effect)
import Effect.Class (liftEffect)
import Grain.Class (class Grain)

-- | The type of component renderer.
-- |
-- | In this monad, you can declare that you use some states and updaters.
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
  { selectValue :: forall p a. Grain p a => p a -> Effect a
  , updateValue :: forall p a. Grain p a => p a -> (a -> a) -> Effect Unit
  }

runRender :: forall a. Render a -> Query -> Effect a
runRender (Render reader) = runReaderT reader <<< QueryBox

-- | Listen a state, then return it.
-- |
-- | If the state is changed, the component will be rerendered.
useValue
  :: forall p a
   . Grain p a
  => p a
  -> Render a
useValue proxy = Render do
  QueryBox query <- ask
  withReaderT (const query)
    $ liftEffect $ query.selectValue proxy

-- | Get an updater of a state.
useUpdater
  :: forall p a
   . Grain p a
  => p a
  -> Render ((a -> a) -> Effect Unit)
useUpdater proxy = Render do
  QueryBox query <- ask
  pure $ query.updateValue proxy
