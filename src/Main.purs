module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Prim.RowList as RL
import Prim.TypeError as TE
import Record as Record
import Type.Prelude (class IsSymbol, SProxy(..))

class IsSingleton (input :: # Type) (name :: Symbol)

instance isSingleton ::
  ( RL.RowToList input rl
  , IsSingletonImpl rl name input
  ) => IsSingleton input name

class IsSingletonImpl (rl :: RL.RowList) (name :: Symbol) (input :: # Type)
  | rl -> name

instance isSingletonImpl :: IsSingletonImpl (RL.Cons name ty RL.Nil) name input
else instance isSingletonImplFail ::
  ( TE.Fail (TE.Above
    (TE.Text "Input was not a singleton record:\n" )
    (TE.Quote { | input }))
  ) => IsSingletonImpl rl name input

-- Record.get alias for fun
myGet
  :: forall input name ty row' row
   . IsSingleton input name
  => IsSymbol name
  => Row.Cons name ty row' row
  => { | input }
  -> { | row }
  -> ty
myGet _ = Record.get (SProxy :: SProxy name)

whatever :: { apple :: String }
whatever = { apple: "banana" }

main :: Effect Unit
main = do
  log $ myGet { apple: unit } whatever
  -- log $ myGet {} whatever -- Input was not a singleton record
  -- log $ myGet { apple: unit, kiwi: unit } whatever -- Input was not a singleton record
