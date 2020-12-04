exception Oops of string

let read_testcase () =
    let n = read_line () |> String.trim |> int_of_string in
    let read_pair () =
        let l = List.map int_of_string (read_line () |> String.trim |> String.split_on_char ' ') in
        match l with
        | a::b::[] -> a, b
        | _ -> raise (Oops "oops!")
    in
    let rec read_pairs n =
        match n with
        | 0 -> []
        | _ -> (read_pair ()) :: read_pairs (n - 1)
    in
    read_pairs n

let rec is_valid testcase =
    let rec lookup k d =
        match d with
        | [] -> None
        | (k',v)::t -> if k=k' then Some v else lookup k t
    in
    match testcase with
    | [] -> true
    | (k',v)::t -> begin
        let value = lookup k' t in
        match value with
        | None -> is_valid t
        | Some(value) ->
            if value=v then is_valid t
            else false
    end

let () =
    let tests = read_line () |> String.trim |> int_of_string in
    let rec run t =
        match t with
        | 0 -> ()
        | _ -> begin
            if (is_valid (read_testcase ())) then print_endline "YES"
            else print_endline "NO";
            run (t - 1)
        end
    in
    run tests
