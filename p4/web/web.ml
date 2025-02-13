open Core
open Util.Error
open Js_of_ocaml
open Lwt.Infix
module P4Parser = Frontend.Parse.Make (Frontend_web.Preprocessor)

let parse_p4 includes filename : El.Ast.program Lwt.t =
  P4Parser.parse_file includes filename

let typecheck includes filename : Il.Ast.program Lwt.t =
  parse_p4 includes filename >>= fun program ->
  Lwt.return (Typing.Typecheck.type_program program)

module StfParser = Stf.Parse.Make (Stf_web.Reader)

let parse_stf filename = StfParser.parse_file filename

let eval (includes : string list) (arch : string) (filename : string)
    (stfname : string) : string Lwt.t =
  typecheck includes filename >>= fun program ->
  Lwt.catch
    (fun () ->
      let cenv, tdenv, fenv, venv, sto =
        Instance.Instantiate.instantiate_program program
      in
      let (module Driver) = Exec.Gen.gen arch in
      parse_stf stfname >>= fun stmts_stf ->
      match stmts_stf with
      | Stf.Ast.Packet (port_in, packet_in) :: _ -> (
          let result =
            Driver.run_packet cenv tdenv fenv venv sto port_in packet_in
          in
          match result with
          | Some (port_out, packet_out) ->
              Lwt.return (Format.sprintf "(%d) %s" port_out packet_out)
          | None -> Lwt.return "packet dropped")
      | _ -> Lwt.return "web interface only supports a single packet input")
    (function
      | ParseErr (msg, info)
      | CheckErr (msg, info)
      | InstErr (msg, info)
      | InterpErr (msg, info) ->
          if Util.Source.is_no_info info then
            Lwt.return (Format.sprintf "Error: %s\n" msg)
          else
            Lwt.return
              (Format.asprintf "Error: %a\n%s\n" Util.Source.pp info msg)
      | DriverErr msg -> Lwt.return (Format.sprintf "Error: %s\n" msg)
      | StfErr msg -> Lwt.return (Format.sprintf "Error: %s\n" msg)
      | exn -> Lwt.fail exn)

let _ =
  Js.export "P4cherry"
    (object%js
       method eval (includes : Js.js_string Js.t Js.js_array Js.t) arch filename
           stfname =
         let includes =
           includes |> Js.to_array |> Array.map ~f:Js.to_string |> Array.to_list
         in
         Promise_lwt.to_promise
           (eval includes (Js.to_string arch) (Js.to_string filename)
              (Js.to_string stfname))
    end)
