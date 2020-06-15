{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "grain"
, license = "MIT"
, repository = "https://github.com/purescript-grain/purescript-grain"
, dependencies = [ "foreign-object", "safely", "unsafe-reference", "web-html" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
