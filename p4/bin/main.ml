open Core
open Util.Error

let version = "0.1"

module P4Parser = Frontend.Parse.Make (Frontend_native.Preprocessor)

let parse_p4 includes filename : El.Ast.program =
  Lwt_main.run (P4Parser.parse_file includes filename)

let roundtrip_p4 includes filename : El.Ast.program =
  Lwt_main.run (P4Parser.roundtrip_file includes filename)

let typecheck includes filename : Il.Ast.program =
  parse_p4 includes filename |> Typing.Typecheck.type_program

let parse_stf filename = Lwt_main.run (Stf.Parse.parse_file filename)

let parse_command =
  Command.basic ~summary:"parse a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and roundtrip_flag =
       flag "-r" no_arg ~doc:"parse, stringify, and parse the program"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program =
           let func = if roundtrip_flag then roundtrip_p4 else parse_p4 in
           func includes filename
         in
         Format.printf "%a\n" El.Pp.pp_program program
       with ParseErr (msg, info) ->
         if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
         else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let typecheck_command =
  Command.basic ~summary:"typecheck a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program = typecheck includes filename in
         Format.printf "%a\n" Il.Pp.pp_program program
       with ParseErr (msg, info) | CheckErr (msg, info) ->
         if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
         else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let instantiate_command =
  Command.basic ~summary:"instantiate a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program = typecheck includes filename in
         Instance.Instantiate.instantiate_program program |> ignore
       with
       | ParseErr (msg, info) | CheckErr (msg, info) | InstErr (msg, info) ->
         if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
         else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg)

let run_command =
  Command.basic ~summary:"run a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and arch = flag "-a" (required string) ~doc:"target architecture"
     and filename = anon ("file.p4" %: string)
     and stfname = anon ("file.stf" %: string) in
     fun () ->
       try
         let program = typecheck includes filename in
         let cenv, tdenv, fenv, venv, sto =
           Instance.Instantiate.instantiate_program program
         in
         let (module Driver) = Exec.Gen.gen arch in
         let stmts_stf = Lwt_main.run (parse_stf stfname) in
         Driver.run cenv tdenv fenv venv sto stmts_stf |> ignore
       with
       | ParseErr (msg, info)
       | CheckErr (msg, info)
       | InstErr (msg, info)
       | InterpErr (msg, info) ->
           if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
           else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg
       | DriverErr msg -> Format.printf "Error: %s\n" msg
       | StfErr msg -> Format.printf "Error: %s\n" msg)

let run_packet_command =
  Command.basic ~summary:"run a p4_16 program with a single packet input"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and arch = flag "-a" (required string) ~doc:"target architecture"
     and port = flag "--port" (required string) ~doc:"port_in"
     and packet = flag "--packet" (required string) ~doc:"packet_in"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       try
         let program = typecheck includes filename in
         let cenv, tdenv, fenv, venv, sto =
           Instance.Instantiate.instantiate_program program
         in
         let (module Driver) = Exec.Gen.gen arch in
         Driver.run_packet cenv tdenv fenv venv sto port packet |> ignore
       with
       | ParseErr (msg, info)
       | CheckErr (msg, info)
       | InstErr (msg, info)
       | InterpErr (msg, info) ->
           if Util.Source.is_no_info info then Format.printf "Error: %s\n" msg
           else Format.printf "Error: %a\n%s\n" Util.Source.pp info msg
       | DriverErr msg -> Format.printf "Error: %s\n" msg
       | StfErr msg -> Format.printf "Error: %s\n" msg)

let command =
  Command.group ~summary:"p4cherry: an interpreter of the p4_16 language"
    [
      ("parse", parse_command);
      ("typecheck", typecheck_command);
      ("instantiate", instantiate_command);
      ("run", run_command);
      ("run-packet", run_packet_command);
    ]

let () = Command_unix.run ~version command
