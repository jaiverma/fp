(* returns an array of n elements *)
let rec make_array n =
    match n with
    | 0 -> []
    | _ -> 1 :: make_array (n - 1)
