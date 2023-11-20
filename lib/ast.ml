type 'a expr =
  | Number of float (* non-negative *)
  | Neg of 'a
  | Add of 'a * 'a
  | Sub of 'a * 'a
  | Mul of 'a * 'a
  | Div of 'a * 'a
  | Exp of 'a * 'a

type evaluated_expr = {
    expr: evaluated_expr expr;
    value: float option
  }

let eval (expr : 'a expr) (getter: 'a -> evaluated_expr): evaluated_expr =
  match expr with
  | Number n -> { expr = Number n; value = Some n }
  | Neg a ->
     let a_eexpr = getter a in
     { expr = Neg a_eexpr;
       value =
         match a_eexpr.value with
         | None -> None
         | Some a_value -> Some (-. a_value) }
  | Add (a, b) ->
     let a_eexpr = getter a in
     let b_eexpr = getter b in
     { expr = Add (a_eexpr, b_eexpr);
       value =
         match a_eexpr.value, b_eexpr.value with
         | Some a_value, Some b_value ->
            (* filter some underflows *)
            if a_value <> 0. && abs_float (b_value /. a_value) > 1e10
               || b_value <> 0. && abs_float (a_value /. b_value) > 1e10 then None
            else Some (a_value +. b_value)
         | _ -> None }
  | Sub (a, b) ->
     let a_eexpr = getter a in
     let b_eexpr = getter b in
     { expr = Sub (a_eexpr, b_eexpr);
       value =
         match a_eexpr.value, b_eexpr.value with
         | Some a_value, Some b_value ->
            (* filter some underflows *)
            if a_value <> 0. && abs_float (b_value /. a_value) > 1e10
               || b_value <> 0. && abs_float (a_value /. b_value) > 1e10 then None
            else Some (a_value -. b_value)
         | _ -> None }
  | Mul (a, b) ->
     let a_eexpr = getter a in
     let b_eexpr = getter b in
     { expr = Mul (a_eexpr, b_eexpr);
       value =
         match a_eexpr.value, b_eexpr.value with
         | Some a_value, Some b_value -> Some (a_value *. b_value)
         | _ -> None }
  | Div (a, b) ->
     let a_eexpr = getter a in
     let b_eexpr = getter b in
     { expr = Div (a_eexpr, b_eexpr);
       value =
         match a_eexpr.value, b_eexpr.value with
         | Some a_value, Some b_value when b_value <> 0. -> Some (a_value /. b_value)
         | _ -> None }
  | Exp (a, b) ->
     let a_eexpr = getter a in
     let b_eexpr = getter b in
     { expr = Exp (a_eexpr, b_eexpr);
       value =
         match a_eexpr.value, b_eexpr.value with
         | Some a_value, Some b_value ->
            let value = a_value ** b_value in
            (* filter out some overflows/underflows *)
            if value = infinity
               || value = neg_infinity
               || value = 0. && a_value <> 0.
               || value = 1. && a_value <> 1. && b_value <> 0. then None
            else begin
                Printf.printf "%f ^ %f = %f\n" a_value b_value value;
                Some value
              end
         | _ -> None }

let evaluate (eexpr: evaluated_expr expr) =
  eval eexpr (fun e -> e)
  
let priority_of_expr = function
  | Number _ -> 5
  | Neg _ -> 4
  | Add _ | Sub _ -> 1
  | Mul _ | Div _ -> 2
  | Exp _ -> 3

type associativity = Left | Right | Both

let associativity_of_expr = function
  | Number _ -> Left (* does it matter? *)
  | Neg _ -> Right
  | Add _ | Mul _ -> Both
  | Sub _ | Div _ -> Left
  | Exp _ -> Right
        
let rec string_of_eexpr caller_priority caller_associativity eexpr =
  let priority = priority_of_expr eexpr.expr in
  let associativity = associativity_of_expr eexpr.expr in
  let left_assoc = associativity = Left || associativity = Both in
  let right_assoc = associativity = Right || associativity = Both in
  let s = match eexpr.expr with
    | Number n -> if n = floor n then Printf.sprintf "%.0f" n else Printf.sprintf "%f" n
    | Neg a -> Printf.sprintf "-%s" (string_of_eexpr priority left_assoc a)
    | Add (a, b) -> Printf.sprintf "%s+%s" (string_of_eexpr priority left_assoc a) (string_of_eexpr priority right_assoc b)
    | Sub (a, b) -> Printf.sprintf "%s-%s" (string_of_eexpr priority left_assoc a) (string_of_eexpr priority right_assoc b)
    | Mul (a, b) -> Printf.sprintf "%s*%s" (string_of_eexpr priority left_assoc a) (string_of_eexpr priority right_assoc b)
    | Div (a, b) -> Printf.sprintf "%s/%s" (string_of_eexpr priority left_assoc a) (string_of_eexpr priority right_assoc b)
    | Exp (a, b) -> Printf.sprintf "%s^%s" (string_of_eexpr priority left_assoc a) (string_of_eexpr priority right_assoc b) in
(*  let s =
    Printf.sprintf "%s[%s]" s
      (match eexpr.value with
       | None -> "None"
       | Some v -> Printf.sprintf "%f" v) in *)
  if priority < caller_priority
     || priority = caller_priority && not caller_associativity then Printf.sprintf "(%s)" s else s

let string_of_eexpr eexpr = string_of_eexpr 0 false eexpr
                          
(* raw AST without partial evaluations, just for unit tests *)
type ast = ast expr
let rec evaluate_ast (expr: ast) = eval expr (fun e -> evaluate_ast e)
let print_evaluate_ast ast = print_endline (string_of_eexpr (evaluate_ast ast))
                          
let%expect_test _ =
  print_evaluate_ast (Number 42.);
  [%expect{|
     42
           |}]
    
let%expect_test _ =
  print_evaluate_ast (Add (Number 1., Number 42.));
  [%expect{|
     1+42
           |}]

let%expect_test _ =
  print_evaluate_ast (Add (Add (Number 1., Number 2.), Number 42.));
  [%expect{|
     1+2+42
           |}]

let%expect_test _ =
  print_evaluate_ast (Add (Number 1., Add (Number 2., Number 42.)));
  [%expect{|
     1+2+42
           |}]

let%expect_test _ =
  print_evaluate_ast (Add (Number 1., Mul (Number 2., Number 42.)));
  [%expect{|
     1+2*42
           |}]

let%expect_test _ =
  print_evaluate_ast (Mul (Number 1., Add (Number 2., Number 42.)));
  [%expect{|
     1*(2+42)
           |}]

let%expect_test _ =
  print_evaluate_ast (Add (Number 1., Sub (Number 2., Number 42.)));
  [%expect{|
     1+2-42
           |}]

let%expect_test _ =
  print_evaluate_ast (Sub (Number 1., Add (Number 2., Number 42.)));
  [%expect{|
     1-(2+42)
           |}]

let%expect_test _ =
  print_evaluate_ast (Div (Div (Number 1., Number 2.), Number 42.));
  [%expect{|
     1/2/42
           |}]

let%expect_test _ =
  print_evaluate_ast (Div (Number 1., Div (Number 2., Number 42.)));
  [%expect{|
     1/(2/42)
           |}]

let%expect_test _ =
  print_evaluate_ast (Exp (Exp (Number 1., Number 2.), Number 42.));
  [%expect{|
     (1^2)^42
           |}]
    
let%expect_test _ =
  print_evaluate_ast (Div (Div (Number 1., Number 2.), Div (Number 3., Number 4.)));
  [%expect{|
     1/2/(3/4)
           |}]

let%expect_test _ =
  print_evaluate_ast (Add (Neg (Number 1.), Neg (Number 2.)));
  [%expect{|
     -1+-2
           |}]


let () =
  assert ({ expr = Number 42.; value = Some 42. } = evaluate (Number 42.))
