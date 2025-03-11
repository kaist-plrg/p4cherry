module type PREPROCESSOR = sig
  val preprocess : string list -> string -> string -> string
end
