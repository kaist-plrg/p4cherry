open Util.Error

let version = "0.1"

(* File collector *)

let rec collect_files ~(suffix : string) dir =
  let files = Sys_unix.readdir dir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let filename = dir ^ "/" ^ file in
      if Sys_unix.is_directory_exn filename && file <> "include" then
        files @ collect_files ~suffix filename
      else if String.ends_with ~suffix filename then files @ [ filename ]
      else files)
    [] files

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_il_command =
  Core.Command.basic
    ~summary:"run static semantics of a p4_16 spec based on backtracking IL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and debug = flag "-dbg" no_arg ~doc:"print debug traces"
     and profile = flag "-profile" no_arg ~doc:"print execution profile" in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let _ =
           Interp_il.Interp.run_typing debug profile spec_il includes_p4
             filename_p4
         in
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl);
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_sl_command =
  Core.Command.basic
    ~summary:"run static semantics of a p4_16 spec based on non-backtracking SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 =
       flag "-p" (required string) ~doc:"p4 file to typecheck"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let _ = Interp_sl.Interp.run_typing spec_sl includes_p4 filename_p4 in
         ()
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let cover_sl_command =
  Core.Command.basic ~summary:"measure phantom coverage of SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and dirname_p4 =
       flag "-d" (required string) ~doc:"p4 directory to typecheck"
     and dirname_closest_miss_opt =
       flag "-cm" (optional string) ~doc:"directory to output closest misses"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let filenames_p4 = collect_files ~suffix:".p4" dirname_p4 in
         Interp_sl.Interp.cover_typing spec_sl includes_p4 filenames_p4
           dirname_closest_miss_opt
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let test_gen_command =
  Core.Command.basic ~summary:"generate tests"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map dir = flag "-d" (required string) ~doc:"output directory" in
     fun () ->
       try
         let gen_results = Test_gen.Gen.generate in
         List.iter
           (fun (filename, contents) ->
             let file = open_out (dir ^ "/" ^ filename) in
             Printf.fprintf file "%s" contents;
             close_out file)
           gen_results
       with Error (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("run-il", run_il_command);
      ("struct", struct_command);
      ("run-sl", run_sl_command);
      ("cover-sl", cover_sl_command);
      ("test-gen", test_gen_command);
    ]

let () = Command_unix.run ~version command
