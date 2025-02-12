module type READER = sig
  val read_all : string -> string Lwt.t
end

