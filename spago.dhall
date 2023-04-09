{ name = "mlogo"
, dependencies =
  [ "ace"
  , "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exists"
  , "foldable-traversable"
  , "functions"
  , "halogen"
  , "halogen-hooks"
  , "halogen-subscriptions"
  , "halogen-svg-elems"
  , "heterogeneous"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "spec"
  , "string-parsers"
  , "strings"
  , "tuples"
  , "web-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
