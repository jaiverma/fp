(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec length_of_list arr =
    match arr with
    | [] -> 0
    | _::xs -> 1 + length_of_list xs

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let arr = read_lines() in
    let ans = length_of_list arr in
    print_int ans
