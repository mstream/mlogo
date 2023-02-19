{ name = "mlogo"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "numbers"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
