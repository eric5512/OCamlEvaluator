(* https://github.com/Chris00/ANSITerminal/blob/master/src/ANSITerminal_unix.ml *)
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
  let saved = ref false in
  let remove (l: char list) (n: int) = List.filteri (fun i _ -> i <> n) l in
  let get_hist pos = List.nth (!history) pos |> String.to_seq |> List.of_seq |> List.rev in
  let rec insert (l: char list) (n: int) (ch: char) = match l with
    | [] when n = 0 -> [ch]
    | [] -> []
    | x::xs -> if n = 0 then ch::l else x::(insert xs (n - 1) ch) in
  let print_and_place str n = 
    if List.length str <> 0 then (print_string (List.to_seq (List.rev str) |> String.of_seq);
    for i = 0 to (List.length str - n - 1) do print_string "\027[D" done) in
  let clear str n = (print_char '\r';
    List.iter (fun x -> print_char ' ') str; print_char ' ';
    print_char '\r') in
  let rec aux arrow acc hpos nch =
    let len = List.length acc in
    match read_key () with
    | Some '\027' ->
      aux true acc hpos nch
    | Some '[' when arrow ->
      aux true acc hpos nch
      | Some 'A' when arrow -> (* Up arrow key *)
      if hpos < List.length !history - 1 then
        ((if not !saved then
          (history := (List.to_seq acc |> String.of_seq)::!history)
        else if hpos = 0 then
          (history := (List.to_seq acc |> String.of_seq)::(List.tl !history)));
        let ns = get_hist (hpos + 1) in
        (clear acc nch;
        print_and_place ns (List.length ns - 1);
        flush Stdlib.stdout;
        saved := true;
        aux false ns (hpos + 1) (List.length ns - 1)))
      else
        aux false acc hpos nch
    | Some 'B' when arrow -> (* Down arrow key *)
      if hpos > 0 then
        let ns = List.nth (!history) (hpos - 1) |> String.to_seq |> List.of_seq |> List.rev in
        (clear acc nch;
        print_and_place ns (List.length ns - 1);
        flush Stdlib.stdout;
        aux false ns (hpos - 1) (List.length ns - 1))
      else
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
      if !saved then history := List.tl !history;
      acc
    | Some '\x7F' -> (* Del key *)
      let acc = remove acc (len - nch) in
      (if nch > 0 then
        (clear acc nch;
        print_and_place acc nch;
        if nch <> len then print_string "\027[D";
        flush Stdlib.stdout;
        aux arrow acc hpos (nch - 1))
      else
        aux arrow acc hpos nch)
    | Some ch ->
      let acc = insert acc (len - nch) ch in
      (if len = nch then 
        (print_char ch;
        flush Stdlib.stdout)
      else
        (clear acc nch;
        print_and_place acc nch;
        print_string "\027[C";
        flush Stdlib.stdout));
      aux arrow acc hpos (nch + 1)
    | None -> acc in
  let str = (aux false [] 0 0) |> List.rev |> List.to_seq |> String.of_seq in
  (if !saved then 
    history := str::(List.tl !history)
  else
    history := str::!history);
  str;;