{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "portal"
, dependencies =
  [ "arrays"
  , "effect"
  , "foreign-object"
  , "grain"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
