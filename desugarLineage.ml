open Utility

class desugar_lineage env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val dp = Sugartypes.dummy_position

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * Sugartypes.iterpatt) = function
    | e -> super#iterpatt e

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | e -> super#phrasenode e
end

let desugar_lineage env = ((new desugar_lineage env) : desugar_lineage :> TransformSugar.transform)
