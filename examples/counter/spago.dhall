{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "counter"
, dependencies =
  [ "effect", "grain", "maybe", "newtype", "prelude", "web-dom", "web-html" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
