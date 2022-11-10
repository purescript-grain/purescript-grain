{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "grain"
, license = "MIT"
, repository = "https://github.com/purescript-grain/purescript-grain"
, dependencies =
  [ "arrays"
  , "effect"
  , "exceptions"
  , "foreign"
  , "functions"
  , "integers"
  , "lazy"
  , "maybe"
  , "nullable"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
