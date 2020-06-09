{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "global-state"
, dependencies = [ "grain", "js-timers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
