{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "svg"
, dependencies =
  [ "effect", "grain", "maybe", "prelude", "web-dom", "web-html" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
