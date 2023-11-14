type expr =
  | Number of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Exp of expr * expr

let rec evaluate expr =
  match expr with
  | Number n -> Some n
  | Add (a, b) ->
     (match evaluate a with
      | None -> None
      | Some a_value ->
         match evaluate b with
         | None -> None
         | Some b_value -> Some (a_value +. b_value))
  | Sub (a, b) ->
     (match evaluate a with
      | None -> None
      | Some a_value ->
         match evaluate b with
         | None -> None
         | Some b_value -> Some (a_value -. b_value))
  | Mul (a, b) ->
     (match evaluate a with
      | None -> None
      | Some a_value ->
         match evaluate b with
         | None -> None
         | Some b_value -> Some (a_value *. b_value))
  | Div (a, b) ->
     (match evaluate a with
      | None -> None
      | Some a_value ->
         match evaluate b with
         | None -> None
         | Some b_value -> 
            if b_value = 0. then None else Some (a_value /. b_value))
  | Exp (a, b) ->
          (match evaluate a with
      | None -> None
      | Some a_value ->
         match evaluate b with
         | None -> None
         | Some b_value -> Some (a_value ** b_value))

let priority_of_expr = function
  | Number _ -> 4
  | Add _ | Sub _ -> 1
  | Mul _ | Div _ -> 2
  | Exp _ -> 3

type associativity = Left | Right

let associativity_of_expr = function
  | Number _ -> Left (* does it matter? *)
  | Add _ | Sub _ | Mul _ | Div _ -> Left
  | Exp _ -> Right
        
let rec string_of_expr caller_priority caller_associativity expr =
  let priority = priority_of_expr expr in
  let associativity = associativity_of_expr expr in
  let s = match expr with
  | Number n -> if n = floor n then Printf.sprintf "%.0f" n else Printf.sprintf "%f" n
  | Add (a, b) -> Printf.sprintf "%s+%s" (string_of_expr priority (associativity = Left) a) (string_of_expr priority (associativity = Right) b)
  | Sub (a, b) -> Printf.sprintf "%s-%s" (string_of_expr priority (associativity = Left) a) (string_of_expr priority (associativity = Right) b)
  | Mul (a, b) -> Printf.sprintf "%s*%s" (string_of_expr priority (associativity = Left) a) (string_of_expr priority (associativity = Right) b)
  | Div (a, b) -> Printf.sprintf "%s/%s" (string_of_expr priority (associativity = Left) a) (string_of_expr priority (associativity = Right) b)
  | Exp (a, b) -> Printf.sprintf "%s^%s" (string_of_expr priority (associativity = Left) a) (string_of_expr priority (associativity = Right) b) in
  if priority < caller_priority
    || priority = caller_priority && not caller_associativity then Printf.sprintf "(%s)" s else s

let string_of_expr expr = string_of_expr 0 false expr
                                                             
let%expect_test _ =
  print_endline (string_of_expr (Number 42.));
  [%expect{|
     42
           |}]
    
let%expect_test _ =
  print_endline (string_of_expr (Add (Number 1., Number 42.)));
  [%expect{|
     1+42
           |}]

let%expect_test _ =
  print_endline (string_of_expr (Add (Add (Number 1., Number 2.), Number 42.)));
  [%expect{|
     1+2+42
           |}]

(* debatable vs 1+2+42 *)
let%expect_test _ =
  print_endline (string_of_expr (Add (Number 1., Add (Number 2., Number 42.))));
  [%expect{|
     1+(2+42)
           |}]

let%expect_test _ =
  print_endline (string_of_expr (Add (Number 1., Mul (Number 2., Number 42.))));
  [%expect{|
     1+2*42
           |}]

let%expect_test _ =
  print_endline (string_of_expr (Mul (Number 1., Add (Number 2., Number 42.))));
  [%expect{|
     1*(2+42)
           |}]

(* debatable vs 1+2-42 *)
let%expect_test _ =
  print_endline (string_of_expr (Add (Number 1., Sub (Number 2., Number 42.))));
  [%expect{|
     1+(2-42)
           |}]

let%expect_test _ =
  print_endline (string_of_expr (Sub (Number 1., Add (Number 2., Number 42.))));
  [%expect{|
     1-(2+42)
           |}]

let%expect_test _ =
  print_endline (string_of_expr (Div (Div (Number 1., Number 2.), Number 42.)));
  [%expect{|
     1/2/42
           |}]

let%expect_test _ =
  print_endline (string_of_expr (Div (Number 1., Div (Number 2., Number 42.))));
  [%expect{|
     1/(2/42)
           |}]

let%expect_test _ =
  print_endline (string_of_expr (Exp (Exp (Number 1., Number 2.), Number 42.)));
  [%expect{|
     (1^2)^42
           |}]
    
let%expect_test _ =
  print_endline (string_of_expr (Div (Div (Number 1., Number 2.), Div (Number 3., Number 4.))));
  [%expect{|
     1/2/(3/4)
           |}]
    
