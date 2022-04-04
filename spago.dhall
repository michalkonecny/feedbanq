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
  , "tuples"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
