open Ast

let () =
  assert (string_of_eexpr (evaluate (Number 42.)) = "42")
