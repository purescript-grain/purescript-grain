{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "portal"
, dependencies = [ "foreign-object", "grain" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
