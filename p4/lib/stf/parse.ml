open Core
open Util.Error

let error = error_stf

let read_all (filename : string) =
  In_channel.read_all filename

let lex (str: string) : Lexing.lexbuf = 
  try Lexing.from_string str
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error

let parse (str: string) =
  try lex str |> Parser.stmts Lexer.token
  with Parser.Error -> Format.asprintf "parser error" |> error

let parse_file (filename : string) =
  read_all filename |> parse
