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
    | x::xs -> if n = 0 then ch::l else x::(insert xs (n - 1) ch) in
  let print_and_place str n = 
    print_string (List.to_seq (List.rev str) |> String.of_seq);
    for i = 0 to (List.length str - n - 1) do print_string "\027[D" done in
  let clear str n = (print_char '\r';
    List.iter (fun x -> print_char ' ') str; print_char ' ';
    print_char '\r') in
  let rec aux arrow acc hpos nch saved =
    let len = List.length acc in
    match read_key () with
    | Some '\027' ->
      aux true acc hpos nch saved
    | Some '[' when arrow ->
      aux true acc hpos nch saved
    | Some 'A' when arrow -> (* Up arrow key *)
      if hpos < List.length !history then
        let ns = List.nth (!history) (hpos) |> String.to_seq |> List.of_seq |> List.rev in
        (if not saved then
          (history := (List.to_seq acc |> String.of_seq)::!history;)
        else if hpos = 0 then
          (history := (List.to_seq acc |> String.of_seq)::(List.tl !history);));
        (clear acc nch;
        print_and_place ns (List.length ns - 1);
        print_string "\027[C";
        flush Stdlib.stdout;
        aux false ns (hpos + 1) (List.length ns - 1) true)
      else
        aux false acc hpos nch saved
    | Some 'B' when arrow -> (* Down arrow key *)
      if hpos > 0 then
        let ns = List.nth (!history) (hpos - 1) |> String.to_seq |> List.of_seq |> List.rev in
        (clear acc nch;
        print_and_place ns (List.length ns - 1);
        print_string "\027[C";
        flush Stdlib.stdout;
        aux false ns (hpos - 1) (List.length ns - 1) saved)
      else
        aux false acc hpos nch saved
    | Some 'C' when arrow -> (* Right arrow key *)
      (if nch <> len then
        (print_string "\027[C";
        flush Stdlib.stdout;
        aux false acc hpos (nch + 1) saved)
      else
        aux false acc hpos nch saved)
    | Some 'D' when arrow -> (* Left arrow key *)
      (if nch <> 0 then
        (print_string "\027[D";
        flush Stdlib.stdout;
        aux false acc hpos (nch - 1) saved)
      else
        aux false acc hpos nch saved)
    | Some '\n' -> (* Enter key *)
      print_endline "";
      if saved then history := List.tl !history;
      acc
    | Some '\x7F' -> (* Del key *)
      let acc = remove acc (len - nch) in
      (if nch > 0 then
        (clear acc nch;
        print_and_place acc nch;
        if nch <> len then print_string "\027[D";
        flush Stdlib.stdout;
        aux arrow acc hpos (nch - 1) saved)
      else
        aux arrow acc hpos nch saved)
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
      aux arrow acc hpos (nch + 1) saved
    | None -> acc in
    let str = (aux false [] 0 0 false) |> List.rev |> List.to_seq |> String.of_seq in
  history := str::!history;
  str;;

let () =
  enable_raw_mode ();
  try while true do (read_line ()) |> print_endline done 
      with e -> (disable_raw_mode (); raise e |> ignore);
  disable_raw_mode ();