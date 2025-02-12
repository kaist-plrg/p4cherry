(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)


 open Core
 open Js_of_ocaml_lwt
 open Yojson.Basic.Util
 open Lwt.Infix

 let preprocess (includes : string list) (filename : string) : string Lwt.t =
  let url = "http://localhost:8080/preprocess" in
  let params =
    String.concat ~sep:"&"
      ([ "filename=" ^ filename ]
       @ List.map ~f:(fun inc -> "includes=" ^ inc) includes)
  in
  let full_url = url ^ "?" ^ params in
  XmlHttpRequest.get full_url >|= fun response ->
  let json_str = response.XmlHttpRequest.content in
  let json = Yojson.Basic.from_string json_str in
  json |> member "output" |> to_string

