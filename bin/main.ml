open Ast
   
let rec solve numbers =
  match numbers with
  | [] -> []
  | h :: q ->
     let everything_as_number = (List.fold_left (fun acc n -> 10. *. acc +. n) 0. numbers) in
     match q with
     | [] -> [Number everything_as_number; Number (-. everything_as_number)]
     | _ -> Number everything_as_number :: Number (-. everything_as_number) :: various_splits [h] q
and various_splits numbers rest =
  match rest with
  | [] -> []
  | h :: q ->
     let solve_left = solve numbers in
     let solve_right = solve rest in
     List.fold_left (fun acc l -> List.fold_left (fun acc r -> Add (l, r) :: Sub (l, r) :: Mul (l, r) :: Div (l, r) :: Exp (l, r) :: acc) acc solve_right) (various_splits (numbers @ [h]) q) solve_left

let numbers = [8.;4.;4.;4.;8.;6.]
;;
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
