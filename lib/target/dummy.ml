open Driver

module Make (Inst : INST) (Interp : INTERP) : ARCH = struct
  let drive _ _ = assert false
  let interp_extern _ = assert false
end
