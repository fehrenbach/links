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
    (* TODO
    | `Iteration (gens, body, cond, orderby) ->
       let (o, gens) = listu o (fun o -> o#iterpatt) gens in
       let (o, body, t) = o#phrase body in
       let (o, cond, _) = option o (fun o -> o#phrase) cond in
       let (o, orderby, _) = option o (fun o -> o#phrase) orderby in
       (o, `Iteration (gens, body, cond, orderby), t) *)
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
