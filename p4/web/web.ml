open Util.Error
open Js_of_ocaml
module P4Parser = Frontend.Parse.Make (Frontend_web.Preprocessor)

let input_name = "input_code.p4"

let typecheck p4_code : Il.Ast.program =
  let program = P4Parser.parse_string input_name p4_code in
  Typing.Typecheck.type_program program

let eval (arch : string) (preprocessed_code : string) (port : string)
    (packet : string) : string Lwt.t =
  Lwt.catch
    (fun () ->
      let program = typecheck preprocessed_code in
      let cenv, tdenv, fenv, venv, sto =
        Instance.Instantiate.instantiate_program program
      in
      let (module Driver) = Exec.Gen.gen arch in
      let result = Driver.run_packet cenv tdenv fenv venv sto port packet in
      match result with
      | Some (port_out, packet_out) ->
          Lwt.return (Format.sprintf "(%d) %s" port_out packet_out)
      | None -> Lwt.return "packet dropped")
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

let eval_stf (arch : string) (preprocessed_code : string) (stf : string) : string Lwt.t =
  Lwt.catch
    (fun () ->
      let program = typecheck preprocessed_code in
      let cenv, tdenv, fenv, venv, sto =
        Instance.Instantiate.instantiate_program program
      in
      let (module Driver) = Exec.Gen.gen arch in
      let stmts_stf = Stf.Parse.parse stf in 
      let _, result = Driver.run_web cenv tdenv fenv venv sto stmts_stf in Lwt.return (Format.sprintf "%s" result)
    )
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
       method eval arch p4_code port packet =
         eval (Js.to_string arch) (Js.to_string p4_code) (Js.to_string port)
           (Js.to_string packet)
        method evalStf arch p4_code stf =
            eval_stf (Js.to_string arch) (Js.to_string p4_code) (Js.to_string stf)
    end)
