import Lake
open Lake DSL

package «soda» where
  -- add package configuration options here

require batteries from git
  "https://github.com/leanprover-community/batteries.git" @ "v4.21.0"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git" @ "v4.21.0"

@[default_target]
lean_lib «Soda» where
  -- add library configuration options here

@[default_target]
lean_exe «soda» where
  root := `Soda.example

