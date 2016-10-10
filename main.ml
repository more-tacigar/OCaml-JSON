let main () =
  let filename = "./sample.json" in
  let result = Json.load_file filename in
  match result with
  | Some v ->
     Json.print_json v
  | None -> ()
    
let () = main ()
