open Core
open Util.Error
open Js_of_ocaml
open Lwt.Infix

module FParser = Frontend.Parse.Make(Frontend_web.Preprocessor)

let parse includes filename : El.Ast.program Lwt.t =
  FParser.parse_file includes filename

let typecheck includes filename : Il.Ast.program Lwt.t =
  parse includes filename >>= fun program ->
  Lwt.return (Typing.Typecheck.type_program program)

module SParser = Stf.Parse.Make(Stf_web.Reader)

let sparse filename =
  SParser.parse_file filename

let eval (includes : string list) (arch : string) (filename : string) (stfname : string) : string Lwt.t =
  typecheck includes filename >>= fun program ->
  Lwt.catch
    (fun () ->
      let cenv, tdenv, fenv, venv, sto =
        Instance.Instantiate.instantiate_program program
      in
      let (module Driver) = Exec.Gen.gen arch in
      sparse stfname >>= fun stmts_stf ->
      let port_out, packet_out = Driver.run_packet cenv tdenv fenv venv sto stmts_stf in
      Lwt.return (Format.sprintf "%s port: %d" packet_out port_out))
    (function
      | ParseErr (msg, info)
      | CheckErr (msg, info)
      | InstErr (msg, info)
      | InterpErr (msg, info) ->
          if Util.Source.is_no_info info then Lwt.return (Format.sprintf "Error: %s\n" msg)
          else Lwt.return (Format.asprintf "Error: %a\n%s\n" Util.Source.pp info msg)
      | DriverErr msg -> Lwt.return (Format.sprintf "Error: %s\n" msg)
      | StfErr msg -> Lwt.return (Format.sprintf "Error: %s\n" msg)
      | exn -> Lwt.fail exn)

let _ =
  Js.export "P4cherry"
    (object%js
      method eval (includes : Js.js_string Js.t Js.js_array Js.t) arch filename stfname =
        let includes = includes |> Js.to_array |> Array.map ~f:Js.to_string |> Array.to_list in
        Promise_lwt.to_promise (eval includes (Js.to_string arch) (Js.to_string filename) (Js.to_string stfname))
    end)
