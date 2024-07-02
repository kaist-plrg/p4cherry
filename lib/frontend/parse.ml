open Util

let preprocess (includes : string) (filename : string) =
  try Some (Preprocessor.preprocess includes filename)
  with _ ->
    Format.eprintf "preprocessor error\n";
    None

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Some (Lexing.from_string file)
  with Lexer.Error s ->
    Format.eprintf "lexer error: %s\n" s;
    None

let parse (lexbuf : Lexing.lexbuf) =
  try Some (Parser.p4program Lexer.lexer lexbuf)
  with Parser.Error ->
    let info = Lexer.info lexbuf in
    Format.eprintf "parser error: %a\n" Source.pp info;
    None

let ( let* ) = Option.bind

let parse_file (includes : string) (filename : string) =
  let* file = preprocess includes filename in
  let* tokens = lex filename file in
  parse tokens

let parse_string (filename : string) (str : string) =
  (* assume str is preprocessed *)
  let* tokens = lex filename str in
  parse tokens
