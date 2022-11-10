let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies = conf.dependencies # [
    "test-unit",
    "foldable-traversable",
    "refs"
  ]
}
