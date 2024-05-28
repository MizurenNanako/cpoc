open Sexplib.Conv

module SemanticCommon = struct
  type identifier = Id of string [@@deriving sexp_of]
end

module CType = struct
  type id_t = SemanticCommon.identifier

  let sexp_of_id_t = SemanticCommon.sexp_of_identifier

  type t =
    | CInt
    | CFloat
    | CArray of t * int
    | CStruct of c_record_type
    | CFunc of t * t list
  [@@deriving sexp_of]

  and c_record_type = (id_t * t) list
end
