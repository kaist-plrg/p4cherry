open Util.Error
open Preprocessor
open Core

let error_info = error_parser_info
let error_no_info = error_parser_no_info

let lex (filename : string) (p4_code : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexing.from_string p4_code
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_info

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.p4program Lexer.lexer lexbuf |> Transform.transform_program with
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      "parser error" |> error_info info
  | _ -> "transform error" |> error_no_info

module Make (Preprocessor : PREPROCESSOR) = struct
  let preprocess (includes : string list) (filename : string) (p4_code : string)
      : string =
    Preprocessor.preprocess includes filename p4_code

  let parse_file (includes : string list) (filename : string) :
      El.Ast.program =
    let p4_code = In_channel.(with_file filename ~f:input_all) in
    let preprocessed_code = preprocess includes filename p4_code in
    let tokens = lex filename preprocessed_code in
    parse tokens

  let parse_string (filename : string)
      (file : string) : El.Ast.program =
    let preprocessed_code = preprocess ["/"] filename file in
    let tokens = lex filename preprocessed_code in
    parse tokens

  let roundtrip_file (includes : string list) (filename : string) =
    let program = parse_file includes filename in
    let program_str = Format.asprintf "%a\n" El.Pp.pp_program program in
    let program' = parse_string filename program_str in
    if not (El.Eq.eq_program program program') then
      "roundtrip error" |> error_no_info;
    program
end
