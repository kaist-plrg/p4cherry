open Util.Error
open Lwt.Infix
open Preprocessor

let error_info = error_parser_info
let error_no_info = error_parser_no_info

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_info

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.p4program Lexer.lexer lexbuf |> Transform.transform_program with
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      "parser error" |> error_info info
  | _ -> "transform error" |> error_no_info

module Make (Preprocessor : PREPROCESSOR) = struct
  let preprocess (includes : string list) (filename : string) : string Lwt.t =
    try Preprocessor.preprocess includes filename
    with _ -> "preprocessor error" |> error_no_info

  let parse_file (includes : string list) (filename : string) : El.Ast.program Lwt.t =
    preprocess includes filename >>= fun file ->
    let tokens = lex filename file in
    Lwt.return (parse tokens)

  let parse_string (filename : string) (str : string) : El.Ast.program Lwt.t =
    (* assume str is preprocessed *)
    let tokens = lex filename str in
    Lwt.return (parse tokens)

  let roundtrip_file (includes : string list) (filename : string) : El.Ast.program Lwt.t =
    parse_file includes filename >>= fun program ->
    let program_str = Format.asprintf "%a\n" El.Pp.pp_program program in
    parse_string filename program_str >>= fun program' ->
    if not (El.Eq.eq_program program program') then
      "roundtrip error" |> error_no_info
    else
      Lwt.return program
end