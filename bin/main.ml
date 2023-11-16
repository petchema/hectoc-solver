open Ast

let memoize (f: ('c -> 'a -> 'b) as 'c) =
  let tbl = Hashtbl.create 13 in
  let self self x =
    match Hashtbl.find_opt tbl x with
    | Some v ->
       v
    | None ->
       let v = f self x in
       Hashtbl.add tbl x v;
       v in
  self

let solve digits =
  (* negative = do we allow leftmost number of the returned expressions to be negative? *)
  (* Used to try to avoid trivial duplicates like a + b and a - (-b), or (-a) * b and a * (-b) *)
  let solve_aux =
    memoize (
        fun self digits negative ->
              let solve_using_no_binary_operator digits negative acc =
                match digits with
                | [] -> acc
                | _ ->
                   let everything_as_number = evaluate (Number (float (List.fold_left (fun acc n -> 10 * acc + n) 0 digits))) in
                   everything_as_number :: (if negative then evaluate (Neg everything_as_number) :: acc else acc) in
              
              let solve_using_binary_operator digits negative acc =
                let with_left_and_right left_digits right_digits negative acc =
                  (* compute all ASTs using left digits, all ASTs using right digits *)
                  (* combine all the results using all available binary operators *)
                  (* try to avoid some trivial solutions by only allowing positive leftmost number in the right ASTs, *)
                  (* (except if top operator is Exp where it bring new solutions) *)
                  let solve_left = self self left_digits negative in
                  let solve_right = self self right_digits false in
                  List.fold_left (fun acc l ->
                      List.fold_left (fun acc r ->
                          evaluate (Add (l, r))
                          :: evaluate (Sub (l, r))
                          :: evaluate (Mul (l, r))
                          :: evaluate (Div (l, r))
                          :: evaluate (Exp (l, r))
                          :: evaluate (Exp (l, evaluate (Neg r)))
                          :: acc
                        ) acc solve_right
                    ) acc solve_left in
      
                let rec vary_splits left_digits right_digits acc =
                  (* consider all the way to split the digits in two non-empty lists *)
                  match right_digits with
                  | [] -> acc
                  | h :: q ->
                     acc
                     |> with_left_and_right left_digits right_digits negative
                     |> vary_splits (left_digits @ [h]) q in
                match digits with
                | h :: ((_ :: _) as q) ->
                   vary_splits [h] q acc
                | _ -> acc in
              
              []
              |> solve_using_binary_operator digits negative
              |> solve_using_no_binary_operator digits negative) in
  solve_aux solve_aux digits true

let list_unique = function
  | [] -> []
  | h :: q ->
     let rec aux curr q =
       match q with
       | [] -> [curr]
       | h :: q when h = curr -> aux curr q
       | h :: q -> curr :: aux h q in
     aux h q

(*
let string_of_digits digits =
  String.concat "" (List.map string_of_int digits)

let () =
  let available_digits = [1;2;3;4;5;6;7;8;9] in
  List.iter (fun a ->
      let digits = [a] in
      List.iter (fun b ->
          let digits = b :: digits in
          List.iter (fun c ->
              let digits = c :: digits in
              List.iter (fun d ->
                  let digits = d :: digits in
                  List.iter (fun e ->
                      let digits = e :: digits in
                      List.iter (fun f ->
                          let digits = f :: digits in
                          let possibilities = solve digits in
                          let answers = List.filter (fun expr -> expr.value = Some 100.) possibilities in
                          let unique_solutions =
                            answers
                            |> List.map string_of_eexpr
                            |> List.sort String.compare
                            |> list_unique in
                          Printf.printf "%s: %d solutions\n%!" (string_of_digits digits) (List.length unique_solutions)
                        ) available_digits
                    ) available_digits
                ) available_digits
            ) available_digits
        ) available_digits
    ) available_digits
 *)
     
let () =
  (* let digits = [8;4;4;4;8;6] *)
  let digits = String.fold_right (fun c acc -> Char.code c - Char.code '0' :: acc) Sys.argv.(1) [] in
  let possibilities = solve digits in
  let answers = List.filter (fun expr -> expr.value = Some 100.) possibilities in
  let unique_solutions =
    answers
    |> List.map string_of_eexpr
    |> List.sort String.compare
    |> list_unique in
  Printf.printf "%d possibilties\n" (List.length possibilities);
  Printf.printf "%d matches\n" (List.length answers);
  Printf.printf "%d unique solutions:\n" (List.length unique_solutions);
  List.iter (Printf.printf "%s\n") unique_solutions
