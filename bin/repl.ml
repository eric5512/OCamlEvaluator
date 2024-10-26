(* https://github.com/Chris00/ANSITerminal/blob/master/src/ANSITerminal_unix.ml *)

type history = string list ref;;
let history: history = ref [];;

let add_to_history (line: string): unit = history := line::!history;;

let remove_from_history (pos: int): unit = 
  let rec aux i l = let t = List.tl l in let h = List.hd l in
    if i = pos then t else h::(aux (i+1) t) in
    aux 0 !history |> ((:=) history);;

let modify_history (pos: int) (new_val: string): unit = 
  let rec aux i l = let t = List.tl l in let h = List.hd l in
    if i = pos then 
      new_val::t
    else 
      if l <> [] then
        h::(aux (i+1) t)
      else
        [] in
    aux 0 !history |> ((:=) history);;

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

let read_line (): string =
  let remove (l: char list) (n: int) = List.filteri (fun i _ -> i <> n) l in
  let get_hist pos = List.nth (!history) pos |> String.to_seq |> List.of_seq |> List.rev in
  let rec insert (l: char list) (n: int) (ch: char) = match l with
    | [] when n = 0 -> [ch]
    | [] -> []
    | x::xs -> if n = 0 then ch::l else x::(insert xs (n - 1) ch) in
  let print_and_place str n = 
    if List.length str <> 0 then (print_string (List.to_seq (List.rev str) |> String.of_seq);
    for _ = 0 to (List.length str - n - 1) do print_string "\027[D" done) in
  let clear str = (print_char '\r';
    List.iter (fun _ -> print_char ' ') str; print_char ' ';
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
        (let ns = get_hist (hpos + 1) in
        (clear acc;
        print_and_place ns (List.length ns - 1);
        flush Stdlib.stdout;
        aux false ns (hpos + 1) (List.length ns - 1)))
      else
        aux false acc hpos nch
    | Some 'B' when arrow -> (* Down arrow key *)
      if hpos > 0 then
        let ns = get_hist (hpos - 1) in
        (clear acc;
        print_and_place ns (List.length ns - 1);
        flush Stdlib.stdout;
        aux false ns (hpos - 1) (List.length ns - 1))
      else
        (
        aux false acc hpos nch)
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
      let acc = remove acc (len - nch) in
      (if nch > 0 then
        (clear acc;
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
        (clear acc;
        print_and_place acc nch;
        print_string "\027[C";
        flush Stdlib.stdout));
      aux arrow acc hpos (nch + 1)
    | None -> acc in
  let str = (aux false [] (-1) 0) |> List.rev |> List.to_seq |> String.of_seq in
  add_to_history str;
  str;;