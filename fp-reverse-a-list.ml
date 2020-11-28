(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec reverse arr =
    match arr with
    | [] -> []
    | x::xs -> reverse xs @ [x]

let () =
    let arr = read_lines() in
    let ans = reverse arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;
