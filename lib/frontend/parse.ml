open Util

let preprocess (includes : string list) (filename : string) =
  try Ok (Preprocessor.preprocess includes filename)
  with _ -> Error "preprocessor error"

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Ok (Lexing.from_string file)
  with Lexer.Error s -> Error (Format.asprintf "lexer error: %s" s)

let parse (lexbuf : Lexing.lexbuf) =
  try
    let program = Parser.p4program Lexer.lexer lexbuf in
    let program = Transform.transform_program program in
    Ok program
  with
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      Error (Format.asprintf "parser error: %a" Source.pp info)
  | _ -> Error "transform error"

let ( let* ) = Result.bind

let parse_file (includes : string list) (filename : string) =
  let* file = preprocess includes filename in
  let* tokens = lex filename file in
  parse tokens

let parse_string (filename : string) (str : string) =
  (* assume str is preprocessed *)
  let* tokens = lex filename str in
  parse tokens

let roundtrip_file (includes : string list) (filename : string) =
  let* program = parse_file includes filename in
  let program_str = Format.asprintf "%a\n" El.Pp.pp_program program in
  let* program' = parse_string filename program_str in
  if El.Eq.eq_program program program' then Ok program
  else (
    El.Eq.eq_program ~dbg:true program program' |> ignore;
    Error "roundtrip error")
