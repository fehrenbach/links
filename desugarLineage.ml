open Utility

let dp = Sugartypes.dummy_position

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

    (* table tname with (oid: Int, _) where constraints from db =>
       (table .., fun () { for (x <-- table ..) [(data = x, prov = [("table" = tname, row = x.oid)])] })
     * In lineage branch, every table has to have a oid: Int column.
     *)

    (* Just recurse, there has to be a better way... *)
    | `Block (bs, e) ->
       let envs = o#backup_envs in
       let (o, bs) = listu o (fun o -> o#binding) bs in
       let (o, e, t) = o#phrase e in
       let _o = o#restore_envs envs in
       {< var_env=var_env >}, `Block (bs, e), t

    | e -> Debug.print ("identity desugaring for: "^Sugartypes.Show_phrasenode.show e);
           super#phrasenode e
end

class desugar_lineage env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val dp = Sugartypes.dummy_position

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Lineage e ->
       Debug.print ("Rewrite to calculate lineage of this:\n"^Sugartypes.Show_phrase.show e);
       let (_, rewritten, t) = ((new lineage_rewriting env) : lineage_rewriting :> TransformSugar.transform)#phrase e in
       Debug.print ("Rewritten to this:\n"^Sugartypes.Show_phrase.show rewritten);
       let pnode : Sugartypes.phrasenode = `Block ([], rewritten) in
       (o, pnode, t)
    | e -> super#phrasenode e
end

let desugar_lineage env = ((new desugar_lineage env) : desugar_lineage :> TransformSugar.transform)
