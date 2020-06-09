{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "grain"
, dependencies = [ "foreign-object", "safely", "unsafe-reference", "web-html" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
