let rec f x arr = match arr with
    | [] -> []
    | h::t ->
        if h < x then h :: f x t
        else f x t

let rec read_lines () =
    try
        let new_number = read_line () in
        (int_of_string new_number) :: (read_lines ())
    with
        End_of_file -> []

let () =
    let n :: arr = read_lines () in
    let res = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) res
