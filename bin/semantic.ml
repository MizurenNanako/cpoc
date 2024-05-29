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
    | CArray of t * int32
    | CStruct of CId.t
    | CFunc of CId.t
  [@@deriving sexp_of]

  let compare = (Stdlib.compare : t -> t -> int)
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
      | None -> CType.CUnknown
  end

  type entries = Entry.t CIdMap.t
  type t = { mutable entries : entries }

  exception StructUndefined of CId.t
  exception StructRedefined of CId.t

  let empty () = { entries = CIdMap.empty }

  let add (name : CId.t) (entry : Entry.t) (table : t) =
    match CIdMap.find_opt name table.entries with
    | None -> table.entries <- CIdMap.add name entry table.entries
    | Some _ -> raise (StructRedefined name)

  let del (name : CId.t) (table : t) =
    table.entries <- CIdMap.remove name table.entries

  let get_field_type (name : CId.t) (field : Entry.field_t)
      (table : t) =
    match CIdMap.find_opt name table.entries with
    | Some x -> Entry.get_field_type field x
    | None -> CType.CUnknown
end

module CFuncTable = struct
  module Entry = struct
    type t = CType.t * CType.t list

    let of_ret_param ret param : t = (ret, param)
  end

  type entries = Entry.t CIdMap.t
  type t = { mutable entries : entries }

  exception FunctionUndefined of CId.t
  exception FunctionRedefined of CId.t

  let empty () = { entries = CIdMap.empty }

  let add (name : CId.t) (entry : Entry.t) (table : t) =
    match CIdMap.find_opt name table.entries with
    | None -> table.entries <- CIdMap.add name entry table.entries
    | Some _ -> raise (FunctionRedefined name)

  let del (name : CId.t) (table : t) =
    table.entries <- CIdMap.remove name table.entries

  let get_type name (table : t) =
    match CIdMap.find_opt name table.entries with
    | Some (a, al) -> (a, al)
    | None -> (CType.CUnknown, [])
end

module CVarTable = struct
  module Entry = struct
    type t = CType.t
  end

  type entries = Entry.t CIdMap.t
  type t = { mutable entries : entries }

  exception VarUndefined of CId.t
  exception VarRedefined of CId.t

  let empty () = { entries = CIdMap.empty }

  let add (name : CId.t) (entry : Entry.t) (table : t) =
    match CIdMap.find_opt name table.entries with
    | None -> table.entries <- CIdMap.add name entry table.entries
    | Some _ -> raise (VarRedefined name)

  let add_int name = add name CType.CInt
  let add_float name = add name CType.CFloat
  let add_errtype name = add name CType.CErrType
  let add_unknown name = add name CType.CUnknown

  let del (name : CId.t) (table : t) =
    table.entries <- CIdMap.remove name table.entries

  let get_type name (table : t) =
    match CIdMap.find_opt name table.entries with
    | Some v -> v
    | None -> CType.CUnknown
end

module CCtx = struct
  exception VarNotArray of CId.t
  exception VarNotStruct of CId.t

  type t = {
    var_tbl : CVarTable.t;
    struc_tbl : CStructTable.t;
    func_tbl : CFuncTable.t;
  }

  let empty () =
    {
      var_tbl = CVarTable.empty ();
      struc_tbl = CStructTable.empty ();
      func_tbl = CFuncTable.empty ();
    }

  let get_var_type name (table : t) =
    CVarTable.get_type name table.var_tbl

  let get_arr_elm_type name (table : t) =
    let a = get_var_type name table in
    match a with CType.CArray (ty, _) -> ty | _ -> CType.CUnknown

  let get_struc_mem_type name field (table : t) =
    let a = get_var_type name table in
    match a with
    | CType.CStruct id ->
        CStructTable.get_field_type id field table.struc_tbl
    | _ -> CType.CUnknown

  let get_structype_mem_type st field (table : t) =
    match st with
    | CType.CStruct id ->
        CStructTable.get_field_type id field table.struc_tbl
    | _ -> CType.CUnknown

  let get_func_ret_param_type name (table : t) =
    CFuncTable.get_type name table.func_tbl
end
