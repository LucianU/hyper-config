{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "ansi"
  , "benchotron"
  , "console"
  , "debug"
  , "effect"
  , "foreign-object"
  , "heterogeneous"
  , "nullable"
  , "numbers"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "spec-quickcheck"
  , "transformers"
  , "validation"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "test_benchmark/**/*.purs" ]
}
