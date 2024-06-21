let () =
  if Array.length Sys.argv < 5 then (
    Format.printf "Usage: %s <target> <include_dir> <filename> <testname>\n"
      Sys.argv.(0);
    exit 1);

  let arch = Sys.argv.(1) in
  let includes = Sys.argv.(2) in
  let filename = Sys.argv.(3) in
  let testname = Sys.argv.(4) in

  Format.printf "Parsing %s with includes %s\n" filename includes;
  let program =
    match Frontend.Parse.parse_file includes filename with
    | Some program -> program
    | None -> failwith "Error while parsing p4."
  in

  Format.printf "Desugaring %s\n" filename;
  let program = Frontend.Desugar.desugar_program program in

  Format.printf "Instantiating and Interpreting %s\n" filename;
  let (module Driver) =
    let open Instance in
    let open Exec in
    let open Target in
    match arch with
    | "v1model" ->
        (module Driver.Make (V1model.Make) (Instantiate.Make) (Interp.Make)
        : Driver.DRIVER)
    | "custom" ->
        (module Driver.Make (Custom.Make) (Instantiate.Make) (Interp.Make)
        : Driver.DRIVER)
    | _ -> failwith "Unknown target: target = v1model | custom"
  in
  let stf =
    match Stf.Parse.parse_file testname with
    | Some stf -> stf
    | None -> failwith "Error while parsing stf."
  in
  Driver.run program stf |> ignore;
  ()
