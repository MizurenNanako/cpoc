module TypeChecker = struct
  module R = Lexical.Range
  module A = Syntactic.AST
  module E = Syntactic.AST.Expr
  module T = Semantic.CType
  module C = Semantic.CCtx
  module S = Semantic.CStructTable
  module F = Semantic.CFuncTable
  module V = Semantic.CVarTable

  let counter = ref 1

  let get_unnamed_id () =
    let tmpid = Printf.sprintf "unnamed_%i" !counter in
    incr counter;
    Semantic.CId.Id tmpid

  let report msg rng = Printf.eprintf "%s in %s" msg (R.dumps rng)

  let idcnv (id : A.identifier) =
    let A.Id id, range = id in
    (Semantic.CId.Id id, range)

  let tagcnv (tag : A.tag) =
    let A.Tag tag, _ = tag in
    idcnv tag

  let idcnvf (id : A.identifier) =
    let A.Id id, range = id in
    (S.Entry.Field id, range)

  let rec expr_check (env : C.t) (expr : E.t) : T.t =
    let expr, rng = expr in
    match expr with
    | ExprBop (_, l, r) ->
        let lt, rt = (expr_check env l, expr_check env r) in
        if lt = rt then lt
        else (
          report "binary operand type not matched" rng;
          T.CErrType)
    | ExprUop (_, a) -> expr_check env a
    | ExprCall (i, args) -> args_check env i args
    | ExprMem (st, field) -> member_check env st field
    | ExprIndex (ar, ind) -> index_check env ar ind
    | AtomID id -> id_check env id
    | AtomINT _ -> T.CInt
    | AtomFLOAT _ -> T.CFloat

  and id_check env id =
    let id, _ = idcnv id in
    C.get_var_type id env

  and args_check env id args =
    let id, rng = idcnv id in
    let (E.Args args) = args in
    let rl = List.map (expr_check env) args in
    let rt, tl = C.get_func_ret_param_type id env in
    match List.compare T.compare rl tl with
    | 0 -> rt
    | _ ->
        report "call parameters type not matched" rng;
        T.CErrType

  and index_check env arr ind =
    let arr, rng = arr in
    match arr with
    | AtomID id ->
        let id, _ = idcnv id in
        let rt = C.get_arr_elm_type id env in
        let e2t = expr_check env ind in
        if e2t = T.CInt then rt
        else (
          report "index type not int" rng;
          T.CErrType)
    | ExprIndex (ar2, ind2) ->
        let rt = index_check env ar2 ind2 in
        let e2t = expr_check env ind in
        if e2t = T.CInt then rt
        else (
          report "index type not int" rng;
          T.CErrType)
    | _ ->
        report "variable not array" rng;
        T.CErrType

  and member_check env st field =
    let ty = expr_check env st in
    let field, _ = idcnvf field in
    C.get_structype_mem_type ty field env

  (* two possibility, the return type or void *)
  let rec stmt_check (env : C.t) (stmt : A.stmt) =
    let stmt, rng = stmt in
    match stmt with
    | StmtRet e -> expr_check env e
    | StmtComp compst -> compst_check env rng compst
    | _ -> T.CVoid

  and compst_check env rng c =
    let A.CompSt (def_list, stmt_list), _ = c in
    deflist_register env def_list;
    stmt_list_check env rng stmt_list

  (* returns return type if any or void *)
  and stmt_list_check env rng sl =
    let (A.StmtList sl) = sl in
    let rec _c acc sl =
      match sl with
      | [] -> acc
      | hd :: tl -> (
          let hdty = stmt_check env hd in
          match hdty with
          | T.CVoid -> _c acc tl
          | T.CErrType -> T.CErrType
          | _ as a ->
              if a = acc then _c acc tl
              else (
                report "return types not same" rng;
                CErrType))
    in
    _c CVoid sl

  (* check and register symbols *)
  and deflist_register env dl =
    let (A.DefList dl) = dl in
    List.iter (def_register env) dl

  and def_register env df =
    let A.Def (sp, A.DecList dcl), rng = df in
    let ty1 = type_from_spec env sp in
    List.iter (dec_register env ty1 rng) dcl

  and dec_register env ty rng dc =
    let dc, _ = dc in
    let id, ty =
      match dc with
      | Dec (vd, None) -> vardec_maker ty rng vd
      | Dec (vd, Some _) -> vardec_maker ty rng vd
    in
    V.add id ty env.var_tbl

  and vardec_maker ty rng vd =
    let vd, _ = vd in
    match vd with
    | VarDec id ->
        let id, _ = idcnv id in
        (id, ty)
    | VarDecArr (vd1, n) ->
        let id, ty = vardec_maker ty rng vd1 in
        (id, T.CArray (ty, n))

  and type_from_spec env sp : T.t =
    let sp, _ = sp in
    match sp with
    | SpecINT -> T.CInt
    | SpecFLOAT -> T.CFloat
    | SpecStruct ss -> type_from_struct_spec env ss

  and type_from_struct_spec env ss =
    let ss, _ = ss in
    match ss with
    | StructSpecShort tag ->
        let tag, _ = tagcnv tag in
        T.CStruct tag
    | StructSpecFull (Some tag, dfl) ->
        let tag, _ = tagcnv tag in
        struct_deflist_register env tag dfl;
        T.CStruct tag
    | StructSpecFull (None, dfl) ->
        let tmpid = get_unnamed_id () in
        incr counter;
        struct_deflist_register env tmpid dfl;
        T.CStruct tmpid

  and struct_deflist_register env id dfl =
    let (A.DefList dfl) = dfl in
    let dfl = List.concat_map (struct_def_maker env) dfl in
    S.add id dfl env.struc_tbl

  and struct_def_maker env df =
    let A.Def (sp, A.DecList dcl), rng = df in
    let ty1 = type_from_spec env sp in
    List.map (struct_dec_maker ty1 rng) dcl

  and struct_dec_maker ty rng dc =
    let dc, _ = dc in
    match dc with
    | A.Dec (vd, None) -> struct_vardec_maker ty rng vd
    | A.Dec (vd, Some _) ->
        report "expression in struct dec" rng;
        struct_vardec_maker ty rng vd

  and struct_vardec_maker ty rng vd =
    let vd, _ = vd in
    match vd with
    | VarDec id ->
        let id, _ = idcnvf id in
        (id, ty)
    | VarDecArr (vd1, n) ->
        let id, ty = struct_vardec_maker ty rng vd1 in
        (id, T.CArray (ty, n))

  let rec extdef_check (env : C.t) (ed : A.extdef) =
    let ed, rng = ed in
    match ed with
    | ExtDefVar (sp, vdl) ->
        let ty1 = type_from_spec env sp in
        let vdl = List.map (vardec_maker ty1 rng) vdl in
        List.iter (fun (id, ty) -> V.add id ty env.var_tbl) vdl
    | ExtDefFun (sp, fd, cps) ->
        let ty1 = type_from_spec env sp in
        let ty2 = compst_check env rng cps in
        if ty1 = ty2 then ()
        else report "function return type not matched" rng;
        fundec_register env ty1 fd

  and fundec_register env ty fd =
    let FunDec (id, VarList pdl), rng = fd in
    let id, _ = idcnv id in
    let tyl = List.map (type_from_param env) pdl in
    let f = (ty, tyl) in
    F.add id f env.func_tbl

  and type_from_param env pd =
    let A.ParamDec (sp, _), _ = pd in
    let ty1 = type_from_spec env sp in
    ty1

  let program_check (prog : A.program) : C.t =
    let (A.Program edl) = prog in
    let ctx = C.empty () in
    List.iter (extdef_check ctx) edl;
    ctx
end
