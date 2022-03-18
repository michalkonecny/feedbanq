{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "feedbanq"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "strings"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
