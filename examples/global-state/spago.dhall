{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "global-state"
, dependencies =
  [ "datetime"
  , "effect"
  , "grain"
  , "js-date"
  , "js-timers"
  , "maybe"
  , "now"
  , "prelude"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
