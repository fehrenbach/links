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

(** Construct singleton lineage record, like
 [(data = $DATA,
   prov = $PROV)] : [(data: $T, prov: [(table: String, row: Int)])]
*)
let lin data prov t : Sugartypes.phrasenode =
  `ListLit ([`RecordLit ([("data", data); ("prov", prov)], None), dp],
            Some (Types.make_lineage_type t))

let empty_prov t : Sugartypes.phrasenode =
  `ListLit ([], Some (Types.make_lineage_type t))

class replace_variable name expr =
object (o : 'self_type)
  inherit SugarTraversals.map as super

  method! phrasenode : Sugartypes.phrasenode -> Sugartypes.phrasenode = function
    | `Var x when x = name -> expr
    | e -> super#phrasenode e
end

(** In expression `in_expr` replace all references to variable named `var_name` with expression `with_expr`.
 Often written as: in_expr[with_expr/var_name]
 or: in_expr[var_name ↦ with_expr]
 *)
let subst : Sugartypes.name -> Sugartypes.phrasenode -> Sugartypes.phrase -> Sugartypes.phrase
  = fun var_name with_expr in_expr ->
  (new replace_variable var_name with_expr)#phrase in_expr

class lineage_rewriting env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! datatype : Types.datatype -> ('self_type * Types.datatype) = function
    (* Would we ever have a list of rows or presences or other things that type_args can be? *) 
    | `Application (abst, [`Type arg]) when abst = Types.list ->
       (o, Types.make_list_type (Types.make_lineage_type arg))
    | t -> (o, t)

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    (* Singleton list *)
    (* [e] : [t] => [(data=e, prov=[])] : [(data:t, prov:[(table: String, row: Int)])] *)
    | `ListLit ([e], Some t) ->
       let (o, e, _) = o#phrase e in
       let (o, t) = o#datatype t in
       let res : Sugartypes.phrasenode = lin e ((empty_prov t), dp) t in
       (* Debug.print ("res: "^Sugartypes.Show_phrasenode.show res); *)
       (o, res, Types.make_list_type (Types.make_lineage_type t))

    (* P[[for (x <- e) return body]] =
         for (y <- P[[e]])
           for (z <- P[[body]] [y.data/x])
           return {<data:z.data, prov:y.prov U z.prov>} *)
    (* (\* TODO generalize to multiple generators *\) *)
    (* TODO `List iterpatt?! *)
    (* Should probably match after recursive call *)
    | `Iteration ([`Table ((`Variable (x, Some xt, _), _) as p, e)], body, cond, orderby) ->
       (* Debug.print ("================================= " ^ x ^ " start ============================="); *)
       let (o, e, _) = o#phrase e in
       let (o, p) = o#pattern p in
       let (o, body, t) = o#phrase body in
       (* Debug.print ("body (" ^ x ^ ") = " ^ (pps PpSugartypes.phrase body)); *)
       (* Debug.print ("body (" ^ x ^ ") : " ^  Types.Show_datatype.show t); *)
       let (o, cond, _) = option o (fun o -> o#phrase) cond in
       let (o, orderby, _) = option o (fun o -> o#phrase) orderby in
       let y = Utility.gensym ~prefix:("y_"^x) () in
       (* Debug.print (x ^ ": "^Types.Show_datatype.show xt); *)
       let yt : Types.datatype = Types.make_lineage_type xt in
       (* Debug.print (y ^ ": " ^Types.Show_datatype.show yt); *)
       let y_in_e : Sugartypes.iterpatt = `List ((`Variable (y, Some yt, dp), dp), (`FnAppl ((`Projection (e, "2"), dp), []), dp)) in
       let zbody = subst x (`Projection ((`Var y, dp), "data")) body in
       let cond = OptionUtils.opt_map (fun cond -> subst x (`Projection ((`Var y, dp), "data")) cond) cond in
       let z = Utility.gensym ~prefix:("z_"^x) () in
       let zt = TypeUtils.element_type t in
       (* Debug.print (z ^ ": " ^ Types.Show_datatype.show zt); *)
       let z_in_zbody = `List ((`Variable (z, Some zt, dp), dp), zbody) in
       let new_body: Sugartypes.phrasenode = `ListLit ([`RecordLit ([("data", (`Projection ((`Var z, dp), "data"), dp));
                                                                     ("prov", (`InfixAppl (([`Type (Types.make_record_type (StringMap.from_alist [("table", Types.string_type);
                                                                                                                                                  ("row", Types.int_type)]));
                                                                                             `Row (Types.make_empty_closed_row ())], `Name "++"),
                                                                                           (`Projection ((`Var y, dp), "prov"), dp),
                                                                                           (`Projection ((`Var z, dp), "prov"), dp)), dp))],
                                                                    None), dp], Some zt) in
       let inner : Sugartypes.phrasenode = `Iteration ([z_in_zbody], (new_body, dp), cond, None) in
       let outer : Sugartypes.phrasenode = `Iteration ([y_in_e], (inner, dp), None, None) in
       let res_t : Types.datatype = Types.make_list_type zt in
       (* Debug.print ("returning body ("^x^") : "^Types.Show_datatype.show res_t); *)
       (* Debug.print ("================================= " ^ x ^ " end ============================="); *)
       (o, outer, res_t)

    | e -> (* Debug.print ("recurse into: "^pps PpSugartypes.phrasenode e); *)
           let (_, e', t') as res = super#phrasenode e in
           (* Debug.print ("from recursive call: "^pps PpSugartypes.phrasenode e' ^ " : " ^ Types.Show_datatype.show t'); *)
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
       (* Debug.print (pps PpSugartypes.iterpatt res); *)
       (o, res)
    | `List (p, e) ->
       let (o, e, _) = o#phrase e in
       let (o, p) = o#pattern p in
       (o, `List (p, e))

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Lineage e ->
       let (var_env, tycon_env, formlet_env, effect_row) = o#backup_envs in
       let env : Types.typing_environment =
         let open Types in
         { var_env = var_env;
           tycon_env = tycon_env;
           effect_row = effect_row } in
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

       let t = Utility.gensym ~prefix:"t" () in
       let lineage : Sugartypes.phrasenode =
         `RecordLit ([("table", name);
                      ("row", (`Projection ((`Var t, dp), "oid"), dp))],
                     None) in
       let lin_t = Types.make_lineage_type read_row in
       let prov_record : Sugartypes.phrasenode =
         `RecordLit ([("data", (`Var t, dp));
                      ("prov", (`ListLit ([(lineage, dp)], Some lin_t), dp))],
                     None) in
       let prov_calc_expr : Sugartypes.phrasenode =
         `ListLit ([prov_record, dp], Some lin_t)
       in
       let pattern : Sugartypes.pattern = (`Variable (t, Some read_row, dp), dp) in
       let funbody : Sugartypes.phrasenode =
         `Iteration ([`Table (pattern, (tablelit, dp))],
                     (prov_calc_expr, dp), None, None) in
       let delayed_type = Types.make_pure_function_type Types.unit_type (Types.make_list_type lin_t) in
       let delayed_prov : Sugartypes.phrasenode =
         `FunLit (Some [(Types.unit_type, Types.make_empty_closed_row ())], `Unl,
                  ([[]], (`Block ([], (funbody, dp)), dp)), `Server) in
       let pair : Sugartypes.phrasenode = `TupleLit [(tablelit, dp); (delayed_prov, dp)] in
       let pair_type = Types.make_tuple_type [tablelit_type; delayed_type] in
       (* Debug.print ("TableLit desugared:\n"^pps PpSugartypes.phrasenode pair); *)
       (o, pair, pair_type)

    | e -> (* Debug.print ("outside lineage transform ->:\n" ^ (pps PpSugartypes.phrasenode e)); *)
           let (_, e', _) as res = super#phrasenode e in
           (* Debug.print ("outside lineage transform <-:\n" ^ (pps PpSugartypes.phrasenode e')); *)
           res
end

let desugar_lineage env = ((new desugar_lineage env) : desugar_lineage :> TransformSugar.transform)
