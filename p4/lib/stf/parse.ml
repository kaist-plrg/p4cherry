open Core
open Util.Error
open Lwt.Infix

let error = error_stf

let read_all (filename : string) =
  let file = In_channel.read_all filename in
  Lwt.return file

let lex (str: string) : Lexing.lexbuf Lwt.t = 
  try Lexing.from_string str |> Lwt.return
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error

let parse (str: string) =
  try lex str >>= fun buf -> Lwt.return (Parser.stmts Lexer.token buf) >>= fun str -> Lwt.return str
  with Parser.Error -> Format.asprintf "parser error" |> error

let lex_1 (filename : string) =
  try read_all filename >>= fun str -> Lwt.return (Lexing.from_string str)
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error

let parse_file (filename : string) =
  read_all filename >>= fun str -> Lwt.return (parse str)
