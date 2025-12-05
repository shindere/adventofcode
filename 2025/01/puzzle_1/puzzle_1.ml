let dial = ref 50

let left () =
  if !dial=0 then dial := 99 else decr dial

let right () =
  if !dial = 99 then dial := 0 else incr dial

type sense = Left | Right

type rotation = sense * int

let line_number = ref 0

let error line msg =
  Printf.eprintf "Line %d: '%s': %s\n%!"
    !line_number line msg;
  exit 2

let get_line () =
  incr line_number;
  match input_line stdin with
    exception End_of_file -> None
    | _ as line -> if (String.length line)<2 then None else (Some line)

let rotation_of_line line =
  (* Printf.eprintf "Parsing line %d: '%s'\n%!" !line_number line; *)
  let l = String.length line in
  let n = int_of_string (String.sub line 1 (l-1)) in
  match line.[0] with
  | 'L' -> (Left, n)
  | 'R' -> (Right, n)
  | _ -> error line "unsupported operation"

let password = ref 0

let perform (sense, n) =
  let op = if sense=Left then left else right in
  for i=1 to n do op (); done;
  if !dial = 0 then incr password


let rec process () = match get_line () with
  | None -> ()
  | Some line ->
    perform (rotation_of_line line);
    process()

let main () =
  process();
  Printf.printf "Parsed %d lines\n%!" !line_number;
  Printf.printf "Password: %d\n%!" !password

let _ = main ()
