open Ast
   
let rec solve numbers negative =
  match numbers with
  | [] -> []
  | h :: q ->
     let everything_as_number = (List.fold_left (fun acc n -> 10. *. acc +. n) 0. numbers) in
     let solutions = match q with
     | [] -> []
     | _ -> various_splits [h] q negative in
     let solutions = if negative then Neg (Number everything_as_number) :: solutions else solutions in
     Number everything_as_number :: solutions
and various_splits numbers rest negative =
  match rest with
  | [] -> []
  | h :: q ->
     let solve_left = solve numbers negative in
     let solve_right = solve rest false in
     List.fold_left (fun acc l -> List.fold_left (fun acc r -> Add (l, r) :: Sub (l, r) :: Mul (l, r) :: Div (l, r) :: Exp (l, r) :: Exp (l, Neg r) :: acc) acc solve_right) (various_splits (numbers @ [h]) q negative) solve_left

let solve numbers = solve numbers true
     
(* let numbers = [8.;4.;4.;4.;8.;6.] *)
let numbers = String.fold_right (fun c acc -> float (Char.code c - Char.code '0') :: acc) Sys.argv.(1) []

let possibilities = solve numbers
let answers = List.filter (fun expr -> evaluate expr = Some 100.) possibilities;;
let list_unique = function
  | [] -> []
  | h :: q ->
     let rec aux curr q =
       match q with
       | [] -> [curr]
       | h :: q when h = curr -> aux curr q
       | h :: q -> curr :: aux h q in
     aux h q
                
let () =
  let unique_solutions =
    answers
    |> List.map string_of_expr
    |> List.sort String.compare
    |> list_unique in
  Printf.printf "%d solutions:\n" (List.length unique_solutions);
  List.iter (Printf.printf "%s\n") unique_solutions
