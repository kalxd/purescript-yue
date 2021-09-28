{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "yue"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-http"
  , "node-streams"
  , "node-url"
  , "nullable"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "refs"
  , "strings"
  , "transformers"
  , "unordered-collections"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "AGPL-3.0-or-later"
, repository = "https://github.com/kalxd/purescript-yue"
}
