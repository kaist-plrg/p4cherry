open Core

let version = "0.1"

let parse includes filename =
  match Frontend.Parse.parse_file includes filename with
  | Some program -> program
  | None -> failwith "Error while parsing p4."

let typecheck includes filename =
  let program = parse includes filename in
  Typing.Typecheck.type_program program

let remove_core decl =
  let filename =
    match Util.Source.at decl with
    | Util.Source.I { filename = f; _ } -> f
    | _ -> ""
  in
  if
    String.compare filename "test/arch/core.p4" = 0
    || String.compare filename "test/arch/v1model.p4" = 0
  then false
  else true

let parse_command =
  Command.basic ~summary:"parse a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program = parse includes filename in
       Format.printf "%a\n" El.Pp.pp_program program)

let typecheck_command =
  Command.basic ~summary:"typecheck a p4_16 program"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program = typecheck includes filename in
       Format.printf "%a\n" Il.Pp.pp_program program)

let compile_command =
  Command.basic ~summary:"compile a p4_16 program to C"
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map includes = flag "-i" (listed string) ~doc:"include paths"
     and filename = anon ("file.p4" %: string) in
     fun () ->
       let program = typecheck includes filename in
       let remove_core_decls = List.filter program ~f:remove_core in
       Compiler.Compile.compile remove_core_decls)
(* Format.printf "%a\n" Il.Pp.pp_program program) *)

let command =
  Command.group ~summary:"p4cherry: an interpreter of the p4_16 language"
    [
      ("parse", parse_command);
      ("typecheck", typecheck_command);
      ("compile", compile_command);
    ]

let () = Command_unix.run ~version command
