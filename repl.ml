let history: string list ref = ref [];;

let enable_raw_mode () =
  let termios = Unix.tcgetattr Unix.stdin in
  let new_termios = { termios with c_icanon = false; c_echo = false } in
  Unix.tcsetattr Unix.stdin TCSANOW new_termios

let disable_raw_mode () =
  let termios = Unix.tcgetattr Unix.stdin in
  let new_termios = { termios with c_icanon = true; c_echo = true } in
  Unix.tcsetattr Unix.stdin TCSANOW new_termios

let read_key () =
  let buf = Bytes.create 1 in
  let n = Unix.read Unix.stdin buf 0 1 in
  if n = 0 then
    None
  else
    Some (Bytes.get buf 0)

let rec read_line (): string =
  let remove (l: char list) (n: int) = List.filteri (fun i _ -> i <> n) l in
  let rec insert (l: char list) (n: int) (ch: char) = match l with
    | [] when n = 0 -> [ch]
    | [] -> []
    | x::xs -> if n = 0 then ch::l else x::(insert l (n - 1) ch) in
  let rec aux arrow acc hpos nch =
    let len = List.length acc in
    match read_key () with
    | Some '\027' ->
      aux true acc hpos nch
    | Some '[' when arrow ->
      aux true acc hpos nch
    | Some 'A' when arrow -> (* Up arrow key *)
      aux false acc hpos nch
    | Some 'B' when arrow -> (* Down arrow key *)
      aux false acc hpos nch
    | Some 'C' when arrow -> (* Right arrow key *)
      (if nch <> len then
        (print_string "\027[C";
        flush Stdlib.stdout;
        aux false acc hpos (nch + 1))
      else
        aux false acc hpos nch)
    | Some 'D' when arrow -> (* Left arrow key *)
      (if nch <> 0 then
        (print_string "\027[D";
        flush Stdlib.stdout;
        aux false acc hpos (nch - 1))
      else
        aux false acc hpos nch)
    | Some '\n' -> (* Enter key *)
      print_endline "";
      acc
    | Some '\x7F' -> (* Del key *)
      (if nch > 0 then
        (print_char '\r';
        List.iter (fun x -> print_char ' ') acc; print_char ' ';
        print_char '\r';
        print_string (List.to_seq (remove acc (len - nch) |> List.rev) |> String.of_seq);
        for i = 0 to (len - nch - 1) do print_string "\027[D" done;
        flush Stdlib.stdout;
        aux arrow (remove acc (len - nch)) hpos (nch - 1))
      else
        aux arrow acc hpos nch)
    | Some ch ->
      print_char ch;
      flush Stdlib.stdout;
      aux arrow (insert acc (len - nch) ch) hpos (nch+1)
    | None -> acc in
  aux false [] 0 0 |> List.rev |> List.to_seq |> String.of_seq;;

let () =
  enable_raw_mode ();
  Printf.printf "\nString %s\n%!" (read_line ());
  disable_raw_mode ();