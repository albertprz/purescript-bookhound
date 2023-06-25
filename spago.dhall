{ name = "bookhound"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tuples"
  , "unfoldable"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/albertprz/purescript-bookhound"
}
