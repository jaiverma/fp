let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let f n arr = (*Complete this function*)
    let rec repeat_x_times x d =
        match x with
        | 0 -> []
        | l -> d :: repeat_x_times (x - 1) d
    in
    let rec repeat_all l acc =
        match l with
        | [] -> acc
        | h::t -> acc @ (repeat_x_times n h) @ (repeat_all t acc)
    in
    repeat_all arr []

let () =
    let n::arr = read_lines() in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;
