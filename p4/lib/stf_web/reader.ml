open Js_of_ocaml_lwt
open Yojson.Basic.Util
open Lwt.Infix

let read_all (filename : string) =
  let url = "http://localhost:8080/stf" ^ "?" ^ "filename=" ^ filename in
  XmlHttpRequest.get url >|= fun response ->
  let json = Yojson.Basic.from_string response.XmlHttpRequest.content in
  json |> member "output" |> to_string
