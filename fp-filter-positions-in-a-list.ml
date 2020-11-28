(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let filter_odd arr =
    let rec filter_odd_impl arr' idx acc =
        match arr' with
        | [] -> acc
        | x::xs -> begin
            if (idx mod 2) == 0 then
                filter_odd_impl xs (idx + 1) (x :: acc)
            else
                filter_odd_impl xs (idx + 1) acc
        end
    in
    List.rev (filter_odd_impl arr 0 [])

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let n::arr = read_lines() in
    let ans = filter_odd arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;
