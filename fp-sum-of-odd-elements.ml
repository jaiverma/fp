(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec sum_odd arr =
    match arr with
    | [] -> 0
    | x::xs ->
        if x mod 2 == 0 then
            sum_odd xs
        else
            x + sum_odd xs

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let arr = read_lines() in
    let ans = sum_odd arr in
    print_int ans
