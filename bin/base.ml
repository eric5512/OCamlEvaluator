open Expression;;

(* TODO: Add optional padding argument and two's complement feature *)
let base_change (base: string) (num: float): string = 
  let int_num = int_of_float num in
  let rec bin_rep num acc = if num == 0 then acc else bin_rep (num lsr 1) ((num mod 2 + 48 |> char_of_int |> String.make 1) ^ acc) in
  if float_of_int int_num <> num then print_string "Warning: Information loss due to int conversion\n";
  match String.lowercase_ascii base with
    | "dec" -> "0d" ^ Printf.sprintf "%d" int_num
    | "oct" -> "0o" ^ Printf.sprintf "%o" int_num
    | "hex" -> "0x" ^ Printf.sprintf "%x" int_num
    | "bin" -> "0b" ^ bin_rep int_num ""
    | _ -> raise (Base_error base);;
