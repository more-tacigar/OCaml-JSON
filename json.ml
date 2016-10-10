let load_file filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  Parser.program Lexer.read lexbuf

let rec print_json (value : Syntax.value) =
  match value with
  | `Assoc x ->
     print_assoc x
  | `List x ->
     print_list x
  | `String x ->
     Printf.printf "\"%s\"" x
  | `Int x ->
     Printf.printf "%d" x
  | `Float x ->
     Printf.printf "%f" x
  | `Bool x ->
     Printf.printf "%s" (string_of_bool x)
  | `Null ->
     Printf.printf "null"
                   
and print_assoc x =
  Printf.printf "{\n";
  List.iter (fun (key, value) ->
      Printf.printf "  \"%s\": " key;
      print_json value;
      Printf.printf ",\n") x;
  Printf.printf "}"

and print_list x =
  Printf.printf "[\n";
  List.iter (fun value ->
      Printf.printf "  ";
      print_json value;
      Printf.printf ",\n") x;
  Printf.printf "]"
      
