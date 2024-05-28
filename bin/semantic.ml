open Sexplib.Conv

module CId = struct
  type t = Id of string [@@deriving sexp_of]

  let compare = (Stdlib.compare : t -> t -> int)
end

module CIdMap = Map.Make (CId)

module CType = struct
  exception MemberNotFound of string

  type t =
    | CUnknown
    | CErrType
    | CVoid
    | CInt
    | CFloat
    | CArray of t * int
    | CStruct of CId.t
    | CFunc of CId.t
  [@@deriving sexp_of]
end

module CStructTable = struct
  module Entry = struct
    type field_t = Field of string [@@deriving sexp_of]
    type t = (field_t * CType.t) list [@@deriving sexp_of]

    exception MemberNotFound of field_t

    let of_two_lists (fieldlist : field_t list)
        (ctypelist : CType.t list) : t =
      List.combine fieldlist ctypelist

    let get_field_type (field : field_t) (entry : t) =
      match List.find_opt (fun (n, _) -> n = field) entry with
      | Some (_, t) -> t
      | None -> raise (MemberNotFound field)
  end

  type entries = Entry.t CIdMap.t
  type t = { mutable entries : entries }

  exception StructUndefined of CId.t

  let empty () = { entries = CIdMap.empty }

  let add (name : CId.t) (entry : Entry.t) (table : t) =
    table.entries <- CIdMap.add name entry table.entries

  let del (name : CId.t) (table : t) =
    table.entries <- CIdMap.remove name table.entries

  let get_field_type (name : CId.t) (field : Entry.field_t)
      (table : t) =
    match CIdMap.find_opt name table.entries with
    | Some x -> Entry.get_field_type field x
    | None -> raise (StructUndefined name)
end

module CFuncTable = struct
  module Entry = struct
    type t = CType.t * CType.t list

    let of_ret_param ret param : t = (ret, param)
  end

  type entries = Entry.t CIdMap.t
  type t = { mutable entries : entries }

  exception FunctionUndefined of CId.t

  let empty () = { entries = CIdMap.empty }

  let add (name : CId.t) (entry : Entry.t) (table : t) =
    table.entries <- CIdMap.add name entry table.entries

  let del (name : CId.t) (table : t) =
    table.entries <- CIdMap.remove name table.entries
end

module CVarTable = struct
  module Entry = struct
    type t = CType.t
  end

  type entries = Entry.t CIdMap.t
  type t = { mutable entries : entries }

  exception VarUndefined

  let empty () = { entries = CIdMap.empty }

  let add (name : CId.t) (entry : Entry.t) (table : t) =
    table.entries <- CIdMap.add name entry table.entries

  let add_int name = add name CType.CInt
  let add_float name = add name CType.CFloat
  let add_errtype name = add name CType.CErrType
  let add_unknown name = add name CType.CUnknown

  let del (name : CId.t) (table : t) =
    table.entries <- CIdMap.remove name table.entries
end
