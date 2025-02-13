open Core
open Util.Error
open Lwt.Infix
open Reader

let error = error_stf

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.stmts Lexer.token lexbuf
  with Parser.Error -> Format.asprintf "parser error" |> error

module Make (Reader : READER) = struct
  let read_all (filename : string) =
    try Reader.read_all filename with _ -> "read error" |> error

  let lex (file : string) =
    try read_all file >>= fun file -> Lwt.return (Lexing.from_string file)
    with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error

  let parse_file (filename : string) =
    lex filename >>= fun tokens -> Lwt.return (parse tokens)
end
