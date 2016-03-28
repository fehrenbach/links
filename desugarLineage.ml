open Utility

let dp = Sugartypes.dummy_position

let pps printer p =
  printer Format.str_formatter p;
  Format.flush_str_formatter ()

(* From TransformSugar.ml *)
let option :
    'self_type ->
  ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
  'a option -> ('self_type * ('a option) * (Types.datatype option))
  =
  fun o f ->
    function
      | None -> (o, None, None)
      | Some x -> let (o, x, t) = f o x in (o, Some x, Some t)

let rec listu :
    'self_type ->
    ('self_type -> 'a -> 'self_type * 'a) ->
  'a list -> 'self_type * 'a list =
  fun o f ->
    function
      | [] -> (o, [])
      | x :: xs ->
          let (o, x) = f o x in
          let (o, xs) = listu o f xs in (o, x::xs)

(** Construct prov part of lineage type
 [(table: String, row: Int)]
*)
let provt =
  Types.make_list_type (Types.make_record_type (StringMap.from_alist [("table", Types.string_type);
                                                                      ("row", Types.int_type)]))

(** Construct lineage type from type of data
 (data: $T,
  prov: [(table: String, row: Int)])
*)
let lint (t : Types.datatype) : Types.datatype =
  Types.make_record_type (StringMap.from_alist [("data", t);
                                                ("prov", provt)])

(** Construct singleton lineage record, like
 [(data = $DATA,
   prov = $PROV)] : [(data: $T, prov: [(table: String, row: Int)])]
*)
let lin data prov t : Sugartypes.phrasenode =
  `ListLit ([`RecordLit ([("data", data); ("prov", prov)], None), dp],
            Some (lint t))

let empty_prov t : Sugartypes.phrasenode =
  `ListLit ([], Some (lint t))

class replace_variable name expr =
object (o : 'self_type)
  inherit SugarTraversals.map as super

  method! phrasenode : Sugartypes.phrasenode -> Sugartypes.phrasenode = function
    | `Var x when x == name -> expr
    | e -> super#phrasenode e
end

(** In expression `in_expr` replace all references to variable named `var_name` with expression `with_expr`.
 Often written as: in_expr[with_expr/var_name]
 or: in_expr[var_name ↦ with_expr]
 *)
let subst : Sugartypes.name -> Sugartypes.phrasenode -> Sugartypes.phrase -> Sugartypes.phrase
  = fun var_name with_expr in_expr ->
  (* TODO I keep cargo-culting this invocation around.
          figure out what the : .. :> business is about and whether I actually need it... *)
  ((new replace_variable var_name with_expr) : replace_variable :> SugarTraversals.map)#phrase in_expr
  (* Would this do? *)
  (* (new replace_variable var_name with_expr)#phrase in_expr *)

class lineage_rewriting env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * Sugartypes.iterpatt) = function
    | `Table (p, e) as _dbg ->
       Debug.print (pps PpSugartypes.iterpatt _dbg);
       let (o, e, _) = o#phrase e in
       let (o, p) = o#pattern p in
       (* In lineage-computing code we take the provenance calculation thunk and force it *)
       let res = `List (p, (`FnAppl ((`Projection (e, "2"), dp), []), dp)) in
       Debug.print (pps PpSugartypes.iterpatt res);
       (o, res)
    | `List (p, e) ->
       let (o, e, _) = o#phrase e in
       let (o, p) = o#pattern p in
       (o, `List (p, e))

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    (* Singleton list *)
    (* [e] : [t] => [(data=e, prov=[])] : [(data:t, prov:[(table: String, row: Int)])] *)
    | `ListLit ([e], Some t) ->
       let (o, e, _) = o#phrase e in
       let (o, t) = o#datatype t in
       let res : Sugartypes.phrasenode = lin e ((empty_prov t), dp) t in
       (* Debug.print ("res: "^Sugartypes.Show_phrasenode.show res); *)
       (o, res, Types.make_list_type (lint t))

    (* P[[for (x <- e1) return e2]] =
         for (y <- P[[e1]])
           for (z <- P[[e2]] [y.data/x])
           return {<data:z.data, prov:y.prov U z.prov>} *)
    (* Is this different for table (<--) comprehensions? *)
    (* The iterpatt is definitely different: we need to project to the view. But does that affect the for? *)
    (* (\* TODO generalize to multiple generators *\) *)
    (* | `Iteration ([gen], body, cond, orderby) -> *)
    (*    let (o, gen) = o#iterpatt gen in *)
    (*    let (o, body, t) = o#phrase body in *)
    (*    let (o, cond, _) = option o (fun o -> o#phrase) cond in *)
    (*    let (o, orderby, _) = option o (fun o -> o#phrase) orderby in *)
    (*    (o, `Iteration ([gen], body, cond, orderby), t) *)

    (* Just recurse, there has to be a better way... *)
    | `Block (bs, e) ->
       let envs = o#backup_envs in
       let (o, bs) = listu o (fun o -> o#binding) bs in
       let (o, e, t) = o#phrase e in
       let _o = o#restore_envs envs in
       {< var_env=var_env >}, `Block (bs, e), t

    | e -> Debug.print ("recurse into: "^pps PpSugartypes.phrasenode e);
           let res = super#phrasenode e in
           Debug.print ("from recursive call: "^pps PpSugartypes.phrasenode e);
           res
end

class desugar_lineage env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val dp = Sugartypes.dummy_position

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * Sugartypes.iterpatt) = function
    | `Table (p, e) as _dbg ->
       Debug.print (pps PpSugartypes.iterpatt _dbg);
       let (o, e, _) = o#phrase e in
       let (o, p) = o#pattern p in
       (* In non-lineage-computing code we take the actual table *)
       let res = `Table (p, (`Projection (e, "1"), dp)) in
       Debug.print (pps PpSugartypes.iterpatt res);
       (o, res)
    | `List (p, e) ->
       let (o, e, _) = o#phrase e in
       let (o, p) = o#pattern p in
       (o, `List (p, e))

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Lineage e ->
       let (var_env, tycon_env, formlet_env, effect_row) = o#backup_envs in
       let env : Types.typing_environment = {
           var_env = var_env;
           tycon_env = tycon_env;
           effect_row = effect_row } in
       (* TODO have to pass the CURRENT environment, which could have been extended by preceding definitions! *)
       Debug.print ("Rewrite to calculate lineage of this:\n"^ pps PpSugartypes.phrase e);
       let (_, rewritten, t) = ((new lineage_rewriting env) : lineage_rewriting :> TransformSugar.transform)#phrase e in
       Debug.print ("Rewritten to this:\n"^pps PpSugartypes.phrase rewritten);
       let pnode : Sugartypes.phrasenode = `Block ([], rewritten) in
       (o, pnode, t)

    (* table name with (oid: Int, _) ... =>
       (table .., fun () { for (x <-- table ..) [(data = x, prov = [("table" = tname, row = x.oid)])] })
     * In lineage branch, every table has to have a oid: Int column.
     *)
    | `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) ->
       let (o, name, _) = o#phrase name in
       let (o, db, _) = o#phrase db in
       let (o, dtype) = o#sugar_datatype dtype in
       let (o, read_row) = o#datatype read_row in
       let (o, write_row) = o#datatype write_row in
       let (o, needed_row) = o#datatype needed_row in
       let tablelit = `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) in
       let tablelit_type = `Table (read_row, write_row, needed_row) in

       let prov_t = Types.make_record_type (StringMap.from_alist [("table", Types.string_type);
                                                                  ("row", Types.int_type)]) in
       let lineage : Sugartypes.phrasenode =
         `RecordLit ([("table", name);
                      ("row", (`Projection ((`Var "t", dp), "oid"), dp))],
                     None) in
       let lin_t = Types.make_record_type (StringMap.from_alist [("data", read_row); ("prov", prov_t)]) in
       let prov_record : Sugartypes.phrasenode =
         `RecordLit ([("data", (`Var "t", dp));
                      ("prov", (`ListLit ([(lineage, dp)], Some lin_t), dp))],
                     None) in
       let prov_calc_expr : Sugartypes.phrasenode =
         `ListLit ([prov_record, dp], Some lin_t)
       in
       let pattern : Sugartypes.pattern = (`Variable ("t", Some read_row, dp), dp) in
       let funbody : Sugartypes.phrasenode =
         `Iteration ([`Table (pattern, (tablelit, dp))],
                     (prov_calc_expr, dp), None, None) in
       let delayed_type = Types.make_pure_function_type Types.unit_type (Types.make_list_type lin_t) in
       let delayed_prov : Sugartypes.phrasenode =
         `FunLit (Some [(Types.unit_type, Types.make_empty_closed_row ())], `Unl,
                  ([[]], (`Block ([], (funbody, dp)), dp)), `Server) in
       let pair : Sugartypes.phrasenode = `TupleLit [(tablelit, dp); (delayed_prov, dp)] in
       let pair_type = Types.make_tuple_type [tablelit_type; delayed_type] in
       Debug.print ("TableLit desugared:\n"^pps PpSugartypes.phrasenode pair);
       (o, pair, pair_type)

    | e -> (* Debug.print ("outside lineage transform ->:\n" ^ (pps PpSugartypes.phrasenode e)); *)
           let (_, e', _) as res = super#phrasenode e in
           (* Debug.print ("outside lineage transform <-:\n" ^ (pps PpSugartypes.phrasenode e')); *)
           res
end

let desugar_lineage env = ((new desugar_lineage env) : desugar_lineage :> TransformSugar.transform)
