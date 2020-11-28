(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec factorial n =
    if n == 0 then 1
    else n * factorial (n - 1)

let term_n n x =
    (x ** float_of_int n) /. float_of_int (factorial n)

let rec e_to_x x =
    let rec e_to_x_n idx n x =
        if idx == n then 0.
        else term_n idx x +. e_to_x_n (idx + 1) n x
    in
    e_to_x_n 0 10 x

let () =
    try let n = read_int() in
        let rec run n =
            if n == 0 then
                ()
            else
                let x = read_float() in
                let ans = e_to_x x in
                Printf.printf "%.4f\n" ans;
                run (n - 1)
        in
        run n
    with
        End_of_file -> ()
