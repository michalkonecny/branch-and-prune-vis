{ name = "halogen-project"
, dependencies = [ "console", "effect", "halogen", "lists", "maybe", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
