(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec print_n_times n s =
    match n with
    | 0 -> ()
    | _ -> begin
        print_endline s;
        print_n_times (n - 1) s
    end

let () =
    let n = read_int() in
    print_n_times n "Hello World"
