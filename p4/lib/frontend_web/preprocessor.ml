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

open Base

let hash = Hashtbl.create (module String)
let input_name = "input.p4"

module HashFS = struct
  let exists path =
    match Hashtbl.find hash path with Some _ -> true | None -> false

  let load path = Hashtbl.find_exn hash path
end

module Pp = P4pp.Eval.Make (HashFS)

let preprocess _ _ p4_code =
  let env = P4pp.Eval.empty input_name [ "/p4/testdata/arch" ] [] in
  let program, _ = Pp.preprocess env input_name p4_code in
  program
