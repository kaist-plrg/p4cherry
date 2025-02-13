open Core

let read_all filename =
  let file = In_channel.read_all filename in
  Lwt.return file
