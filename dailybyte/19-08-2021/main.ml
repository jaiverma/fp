open Base
open Stdio

let decode s =
  let rec decode_impl s acc =
    match s with
    | n :: c :: xs ->
      let n = Int.of_string n in
      decode_impl xs @@ acc @ List.init n ~f:(fun _ -> c)
    | [] -> acc
    | _ -> failwith "..."
  in
  let chars = String.to_list s |> List.map ~f:String.of_char in
  String.concat ~sep:"" @@ decode_impl chars []
;;

let () =
  let ans = decode "3a2b1c" in
  printf "%s\n" ans
;;
