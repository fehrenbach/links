%{

open Utility
open List
open Sugartypes

(* Generation of fresh type variables *)

let type_variable_counter = ref 0

let fresh_type_variable : subkind option -> datatype =
  function subkind ->
    incr type_variable_counter; `TypeVar ("_" ^ string_of_int (!type_variable_counter), subkind, `Flexible)

let fresh_rigid_type_variable : subkind option -> datatype =
  function subkind ->
    incr type_variable_counter; `TypeVar ("_" ^ string_of_int (!type_variable_counter), subkind, `Rigid)

let fresh_row_variable : subkind option -> row_var =
  function subkind ->
    incr type_variable_counter; `Open ("_" ^ string_of_int (!type_variable_counter), subkind, `Flexible)

let fresh_rigid_row_variable : subkind option -> row_var =
  function subkind ->
    incr type_variable_counter; `Open ("_" ^ string_of_int (!type_variable_counter), subkind, `Rigid)

let fresh_presence_variable : subkind option -> fieldspec =
  function subkind ->
    incr type_variable_counter; `Var ("_" ^ string_of_int (!type_variable_counter), subkind, `Flexible)

let fresh_rigid_presence_variable : subkind option -> fieldspec =
  function subkind ->
    incr type_variable_counter; `Var ("_" ^ string_of_int (!type_variable_counter), subkind, `Rigid)

let ensure_match (start, finish, _) (opening : string) (closing : string) = function
  | result when opening = closing -> result
  | _ -> raise (ConcreteSyntaxError ("Closing tag '" ^ closing ^ "' does not match start tag '" ^ opening ^ "'.",
                                     (start, finish, None)))

let pos () : Sugartypes.position = Parsing.symbol_start_pos (), Parsing.symbol_end_pos (), None

let default_fixity = 9

let annotate (signame, datatype) : _ -> binding =
  let checksig (signame, _) name =
    if signame <> name then
      raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `"^ name ^"'.",
                pos ())) in
    function
      | `Fun ((name, bpos), lin, phrase, location, dpos) ->
          let _ = checksig signame name in
            `Fun ((name, None, bpos), lin, ([], phrase), location, Some datatype), dpos
      | `Var (((name, bpos), phrase, location), dpos) ->
          let _ = checksig signame name in
            `Val ([], (`Variable (name, None, bpos), dpos), phrase, location, Some datatype), dpos

let primary_kind_of_string pos =
  function
  | "Type" -> `Type
  | "Row" -> `Row
  | "Presence" -> `Presence
  | pk -> raise (ConcreteSyntaxError ("Invalid primary kind: " ^ pk, pos))

let linearity_of_string pos =
  function
  | "Any" -> `Any
  | "Unl" -> `Unl
  | lin -> raise (ConcreteSyntaxError ("Invalid kind linearity: " ^ lin, pos))

let restriction_of_string pos =
  function
  | "Any" -> `Any
  | "Base" -> `Base
  | "Session" -> `Session
  | rest -> raise (ConcreteSyntaxError ("Invalid kind restriction: " ^ rest, pos))

let full_kind_of pos prim lin rest =
  let p = primary_kind_of_string pos prim in
  let l = linearity_of_string pos lin in
  let r = restriction_of_string pos rest in
  p, Some (l, r)

let full_subkind_of pos lin rest =
  let l = linearity_of_string pos lin in
  let r = restriction_of_string pos rest in
  Some (l, r)

(* In kind and subkind abbreviations, we aim to provide the most
common case. For everything except session types, the default
linearity is `Unl and the default restriction is `Any. For session
types the default linearity is `Any. *)

(* Currently "Any" means `Any,`Any, but it is probably advisable to
change "Any" to something more evocative of linearity - "Lin"
perhaps. *)

let kind_of pos =
  function
  (* primary kind abbreviation  *)
  | "Type" -> `Type, None
  | "Row" -> `Row, None
  | "Presence" -> `Presence, None
  (* subkind of type abbreviations *)
  | "Any" -> `Type, Some (`Any, `Any)
  | "Base" -> `Type, Some (`Unl, `Base)
  | "Session" -> `Type, Some (`Any, `Session)
  | k -> raise (ConcreteSyntaxError ("Invalid kind: " ^ k, pos))

let subkind_of pos =
  function
  (* subkind abbreviations *)
  | "Any" -> Some (`Any, `Any)
  | "Base" -> Some (`Unl, `Base)
  | "Session" -> Some (`Any, `Session)
  | sk -> raise (ConcreteSyntaxError ("Invalid subkind: " ^ sk, pos))

let attach_kind pos (t, k) = (t, k, `Rigid)

let attach_subkind_helper update pos sk = update sk

let attach_subkind pos (t, subkind) =
  let update sk =
    match t with
    | `TypeVar (x, _, freedom) ->
       `TypeVar (x, sk, freedom)
    | _ -> assert false
  in
    attach_subkind_helper update pos subkind

let attach_session_subkind pos (t, subkind) =
  let update sk =
    match t with
    | `TypeVar (x, _, freedom) ->
       `TypeVar (x, sk, freedom)
    | _ -> assert false
  in
    attach_subkind_helper update pos subkind

let attach_row_subkind pos (r, subkind) =
  let update sk =
    match r with
    | `Open (x, _, freedom) ->
       `Open (x, sk, freedom)
    | _ -> assert false
  in
    attach_subkind_helper update pos subkind

let row_with field (fields, row_var) = field::fields, row_var

(* this preserves 1-tuples *)
let make_tuple pos =
  function
    | [e] -> `RecordLit ([("1", e)], None), pos
    | es -> `TupleLit es, pos

let labels = List.map fst

let parseRegexFlags f =
  let rec asList f i l =
    if (i == String.length f) then
      List.rev l
    else
      asList f (i+1) ((String.get f i)::l) in
    List.map (function 'l' -> `RegexList | 'n' -> `RegexNative | 'g' -> `RegexGlobal) (asList f 0 [])

let datatype d = d, None

%}

%token END
%token EQ IN
%token FUN LINFUN RARROW LOLLI FATRARROW MINUSLBRACE VAR OP
%token SQUIGRARROW SQUIGLOLLI TILDE
%token IF ELSE
%token MINUS MINUSDOT
%token SWITCH RECEIVE CASE SPAWN SPAWNANGEL SPAWNCLIENT SPAWNDEMON SPAWNWAIT
%token OFFER SELECT
%token LPAREN RPAREN
%token LBRACE RBRACE LBRACEBAR BARRBRACE LQUOTE RQUOTE
%token RBRACKET LBRACKET LBRACKETBAR BARRBRACKET
%token LBRACKETPLUSBAR BARPLUSRBRACKET
%token LBRACKETAMPBAR BARAMPRBRACKET
%token LEFTTRIANGLE RIGHTTRIANGLE NU
%token FOR LARROW LLARROW WHERE FORMLET PAGE
%token LRARROW
%token COMMA VBAR DOT DOTDOT COLON COLONCOLON COLONCOLONCOLON
%token TABLE TABLEHANDLE FROM DATABASE QUERY WITH YIELDS ORDERBY 
%token TABLEKEYS /* SAND */
%token UPDATE DELETE INSERT VALUES SET RETURNING
%token READONLY DEFAULT
%token ESCAPE
%token CLIENT SERVER NATIVE
%token SEMICOLON
%token TRUE FALSE
%token BARBAR AMPAMP
%token <int> UINTEGER
%token <float> UFLOAT
%token <string> STRING CDATA REGEXREPL
%token <char> CHAR
%token <string> QUALIFIEDVARIABLE VARIABLE CONSTRUCTOR KEYWORD PERCENTVAR
%token <string> QUALIFIEDMODULE LXML ENDTAG
%token RXML SLASHRXML
%token MU FORALL ALIEN SIG OPEN
%token MODULE
%token BANG QUESTION
%token PERCENT EQUALSTILDE PLUS STAR ALTERNATE SLASH SSLASH CARET DOLLAR
%token <char*char> RANGE
%token <string> QUOTEDMETA
%token <string> SLASHFLAGS
%token UNDERSCORE AS
%token <[`Left|`Right|`None|`Pre|`Post] -> int -> string -> unit> INFIX INFIXL INFIXR PREFIX POSTFIX
%token TYPENAME
%token TYPE ROW PRESENCE
%token <string> PREFIXOP POSTFIXOP
%token <string> INFIX0 INFIXL0 INFIXR0
%token <string> INFIX1 INFIXL1 INFIXR1
%token <string> INFIX2 INFIXL2 INFIXR2
%token <string> INFIX3 INFIXL3 INFIXR3
%token <string> INFIX4 INFIXL4 INFIXR4
%token <string> INFIX5 INFIXL5 INFIXR5
%token <string> INFIX6 INFIXL6 INFIXR6
%token <string> INFIX7 INFIXL7 INFIXR7
%token <string> INFIX8 INFIXL8 INFIXR8
%token <string> INFIX9 INFIXL9 INFIXR9

%start just_datatype
%start interactive
%start file

%type <Sugartypes.binding list * Sugartypes.phrase option> file
%type <Sugartypes.datatype> datatype
%type <Sugartypes.datatype> just_datatype
%type <Sugartypes.sentence> interactive
%type <Sugartypes.regex> regex_pattern_alternate
%type <Sugartypes.regex> regex_pattern
%type <Sugartypes.regex list> regex_pattern_sequence
%type <Sugartypes.pattern> pattern
%type <(Sugartypes.name * Sugartypes.position) * Sugartypes.declared_linearity * Sugartypes.funlit * Sugartypes.location * Sugartypes.position> tlfunbinding
%type <Sugartypes.phrase> postfix_expression
%type <Sugartypes.phrase> primary_expression
%type <Sugartypes.phrase> atomic_expression
%type <Sugartypes.constant * Sugartypes.position> constant
%type <(string * Sugartypes.phrase list) list> attr_list
%type <Sugartypes.phrase list> attr_val
%type <Sugartypes.binding> binding

%%

interactive:
| nofun_declaration                                            { `Definitions [$1] }
| fun_declarations SEMICOLON                                   { `Definitions $1 }
| SEMICOLON                                                    { `Definitions [] }
| exp SEMICOLON                                                { `Expression $1 }
| directive                                                    { `Directive $1 }
| END                                                          { `Directive ("quit", []) (* rather hackish *) }

file:
| preamble declarations exp END                                { $1 @ $2, Some $3 }
| preamble exp END                                             { $1, Some $2 }
| preamble declarations END                                    { $1 @ $2, None }

directive:
| KEYWORD args SEMICOLON                                       { ($1, $2) }

args:
| /* empty */                                                  { [] }
| arg args                                                     { $1 :: $2 }

arg:
| STRING                                                       { $1 }
| VARIABLE                                                     { $1 }
| CONSTRUCTOR                                                  { $1 }
| UINTEGER                                                     { string_of_int $1 }
| UFLOAT                                                       { string_of_float' $1 }
| TRUE                                                         { "true" }
| FALSE                                                        { "false" }

var:
| VARIABLE                                                     { $1, pos() }

preamble:
| /* empty */                                                  { [] }

declarations:
| declarations declaration                                     { $1 @ [$2] }
| declaration                                                  { [$1] }

declaration:
| fun_declaration                                              { $1 }
| nofun_declaration                                            { $1 }

nofun_declaration:
| ALIEN VARIABLE var COLON datatype SEMICOLON                  { let (name, name_pos) = $3 in
                                                                   `Foreign ((name, None, name_pos), $2, datatype $5), pos() }
| fixity perhaps_uinteger op SEMICOLON                         { let assoc, set = $1 in
                                                                   set assoc (from_option default_fixity $2) (fst $3);
                                                                   (`Infix, pos()) }
| tlvarbinding SEMICOLON                                       { let ((d,dpos),p,l), pos = $1
                                                                 in `Val ([], (`Variable (d, None, dpos), pos),p,l,None), pos }
| signature tlvarbinding SEMICOLON                             { annotate $1 (`Var $2) }
| typedecl SEMICOLON                                           { $1 }

| links_module                                                 { $1 }
| links_open                                                   { $1 }


links_module:
| MODULE module_name moduleblock                               { let (mod_name, name_pos) = $2 in
                                                                 `Module (mod_name, (`Block $3, name_pos)), name_pos }
module_name:
| CONSTRUCTOR                                                  { $1 , pos () }

fun_declarations:
| fun_declarations fun_declaration                             { $1 @ [$2] }
| fun_declaration                                              { [$1] }

fun_declaration:
| tlfunbinding                                                 { let ((d,dpos),lin,p,l,pos) = $1
                                                                 in `Fun ((d, None, dpos),lin,([],p),l,None), pos }
| signature tlfunbinding                                       { annotate $1 (`Fun $2) }

perhaps_uinteger:
| /* empty */                                                  { None }
| UINTEGER                                                     { Some $1 }

prefixop:
| PREFIXOP                                                     { $1, pos() }

postfixop:
| POSTFIXOP                                                    { $1, pos() }

tlfunbinding:
| FUN var arg_lists perhaps_location block                     { ($2, `Unl, ($3, (`Block $5, pos ())), $4, pos ()) }
| LINFUN var arg_lists perhaps_location block                  { ($2, `Lin, ($3, (`Block $5, pos ())), $4, pos ()) }
| OP pattern op pattern perhaps_location block                 { ($3, `Unl, ([[$2; $4]], (`Block $6, pos ())), $5, pos ()) }
| OP prefixop pattern perhaps_location block                   { ($2, `Unl, ([[$3]], (`Block $5, pos ())), $4, pos ()) }
| OP pattern postfixop perhaps_location block                  { ($3, `Unl, ([[$2]], (`Block $5, pos ())), $4, pos ()) }

tlvarbinding:
| VAR var perhaps_location EQ exp                              { ($2, $5, $3), pos() }

signature:
| SIG var COLON datatype                                       { $2, datatype $4 }
| SIG op COLON datatype                                        { $2, datatype $4 }

typedecl:
| TYPENAME CONSTRUCTOR typeargs_opt EQ datatype                { `Type ($2, $3, datatype $5), pos()  }

typeargs_opt:
| /* empty */                                                  { [] }
| LPAREN varlist RPAREN                                        { $2 }

kind:
| COLONCOLON CONSTRUCTOR LPAREN CONSTRUCTOR COMMA CONSTRUCTOR RPAREN
                                                               { full_kind_of (pos()) $2 $4 $6 }
| COLONCOLON CONSTRUCTOR                                       { kind_of (pos()) $2 }

subkind:
| COLONCOLON LPAREN CONSTRUCTOR COMMA CONSTRUCTOR RPAREN       { full_subkind_of (pos()) $3 $5 }
| COLONCOLON CONSTRUCTOR                                       { subkind_of (pos()) $2 }

typearg:
| VARIABLE                                                     { (($1, (`Type, None), `Rigid), None) }
| VARIABLE kind                                                { (attach_kind (pos()) ($1, $2), None) }

varlist:
| typearg                                                      { [$1] }
| typearg COMMA varlist                                        { $1 :: $3 }

fixity:
| INFIX                                                        { `None, $1 }
| INFIXL                                                       { `Left, $1 }
| INFIXR                                                       { `Right, $1 }
| PREFIX                                                       { `Pre, $1 }
| POSTFIX                                                      { `Post, $1 }

perhaps_location:
| SERVER                                                       { `Server }
| CLIENT                                                       { `Client }
| NATIVE                                                       { `Native }
| /* empty */                                                  { `Unknown }

constant:
| UINTEGER                                                     { `Int $1    , pos() }
| UFLOAT                                                       { `Float $1  , pos() }
| STRING                                                       { `String $1 , pos() }
| TRUE                                                         { `Bool true , pos() }
| FALSE                                                        { `Bool false, pos() }
| CHAR                                                         { `Char $1   , pos() }

atomic_expression:
| VARIABLE                                                     { `Var $1, pos() }
| QUALIFIEDVARIABLE                                            { `Var $1, pos() }
| constant                                                     { let c, p = $1 in `Constant c, p }
| parenthesized_thing                                          { $1 }
/* HACK: allows us to support both mailbox receive syntax
and receive for session types. */
| RECEIVE                                                      { `Var "receive", pos() }

cp_name:
| VARIABLE                                                     { $1, None, pos () }

cp_label:
| CONSTRUCTOR                                                  { $1 }

cp_case:
| CASE cp_label RARROW cp_expression                           { $2, $4 }

cp_cases:
| cp_case                                                      { [$1] }
| cp_case cp_cases                                             { $1 :: $2 }

perhaps_cp_cases:
| /* empty */                                                  { [] }
| cp_cases                                                     { $1 }

perhaps_name:
|                                                              { None }
| cp_name                                                      { Some $1 }

/* currently unused */
/*
perhaps_exp:
|                                                              { None }
| exp                                                          { Some $1 }
*/

cp_expression:
| LBRACE block_contents RBRACE                                 { `Unquote $2, pos () }
| cp_name LPAREN perhaps_name RPAREN DOT cp_expression         { `Grab ((fst3 $1, None), $3, $6), pos () }
| cp_name LBRACKET exp RBRACKET DOT cp_expression              { `Give ((fst3 $1, None), Some $3, $6), pos () }
| cp_name LBRACKET RBRACKET                                    { `GiveNothing $1, pos () }
| OFFER cp_name LBRACE perhaps_cp_cases RBRACE                 { `Offer ($2, $4), pos () }
| cp_label cp_name DOT cp_expression                           { `Select ($2, $1, $4), pos () }
| cp_name LRARROW cp_name                                      { `Fuse ($1, $3), pos () }
| NU cp_name DOT LPAREN cp_expression VBAR cp_expression RPAREN { `Comp ($2, $5, $7), pos () }

primary_expression:
| atomic_expression                                            { $1 }
| LBRACKET RBRACKET                                            { `ListLit ([], None), pos() }
| LBRACKET exps RBRACKET                                       { `ListLit ($2, None), pos() }
| LBRACKET exp DOTDOT exp RBRACKET                             { `RangeLit($2, $4), pos() }
| xml                                                          { $1 }
| FUN arg_lists block                                          { `FunLit (None, `Unl, ($2, (`Block $3, pos ())), `Unknown), pos() }
| LINFUN arg_lists block                                       { `FunLit (None, `Lin, ($2, (`Block $3, pos ())), `Unknown), pos() }
| LEFTTRIANGLE cp_expression RIGHTTRIANGLE                     { `CP $2, pos () }

constructor_expression:
| CONSTRUCTOR                                                  { `ConstructorLit($1, None, None), pos() }
| CONSTRUCTOR parenthesized_thing                              { `ConstructorLit($1, Some $2, None), pos() }

parenthesized_thing:
| LPAREN binop RPAREN                                          { `Section $2, pos() }
| LPAREN DOT record_label RPAREN                               { `Section (`Project $3), pos() }
| LPAREN RPAREN                                                { `RecordLit ([], None), pos() }
| LPAREN labeled_exps VBAR exp RPAREN                          { `RecordLit ($2, Some $4), pos() }
| LPAREN labeled_exps RPAREN                                   { `RecordLit ($2, None),               pos() }
| LPAREN exps RPAREN                                           { `TupleLit ($2), pos() }
| LPAREN exp WITH labeled_exps RPAREN                          { `With ($2, $4), pos() }

binop:
| MINUS                                                        { `Minus }
| MINUSDOT                                                     { `FloatMinus }
| op                                                           { `Name (fst $1) }

op:
| INFIX0                                                       { $1, pos() }
| INFIXL0                                                      { $1, pos() }
| INFIXR0                                                      { $1, pos() }
| INFIX1                                                       { $1, pos() }
| INFIXL1                                                      { $1, pos() }
| INFIXR1                                                      { $1, pos() }
| INFIX2                                                       { $1, pos() }
| INFIXL2                                                      { $1, pos() }
| INFIXR2                                                      { $1, pos() }
| INFIX3                                                       { $1, pos() }
| INFIXL3                                                      { $1, pos() }
| INFIXR3                                                      { $1, pos() }
| INFIX4                                                       { $1, pos() }
| INFIXL4                                                      { $1, pos() }
| INFIXR4                                                      { $1, pos() }
| INFIX5                                                       { $1, pos() }
| INFIXL5                                                      { $1, pos() }
| INFIXR5                                                      { $1, pos() }
| INFIX6                                                       { $1, pos() }
| INFIXL6                                                      { $1, pos() }
| INFIXR6                                                      { $1, pos() }
| INFIX7                                                       { $1, pos() }
| INFIXL7                                                      { $1, pos() }
| INFIXR7                                                      { $1, pos() }
| INFIX8                                                       { $1, pos() }
| INFIXL8                                                      { $1, pos() }
| INFIXR8                                                      { $1, pos() }
| INFIX9                                                       { $1, pos() }
| INFIXL9                                                      { $1, pos() }
| INFIXR9                                                      { $1, pos() }

postfix_expression:
| primary_expression                                           { $1 }
| primary_expression POSTFIXOP                                 { `UnaryAppl (([], `Name $2), $1), pos() }
| block                                                        { `Block $1, pos () }
| SPAWN perhaps_location block                                 { `Spawn (`Demon, $2, (`Block $3, pos()), None), pos () }
| SPAWNCLIENT perhaps_location block                           { `Spawn (`Client, $2, (`Block $3, pos()), None), pos () }
| SPAWNANGEL perhaps_location block                            { `Spawn (`Angel, $2, (`Block $3, pos()), None), pos () }
| SPAWNDEMON perhaps_location block                            { `Spawn (`Demon, $2, (`Block $3, pos()), None), pos () }
| SPAWNWAIT perhaps_location block                             { `Spawn (`Wait,  $2, (`Block $3, pos()), None), pos () }
| QUERY block                                                  { `Query (None, (`Block $2, pos ()), None), pos () }
| QUERY LBRACKET exp RBRACKET block                            { `Query (Some ($3,
                                                                               (`Constant (`Int 0), pos ())),
                                                                         (`Block $5, pos ()), None), pos () }
| QUERY LBRACKET exp COMMA exp RBRACKET block                  { `Query (Some ($3, $5), (`Block $7, pos ()), None), pos () }
| postfix_expression arg_spec                                  { `FnAppl ($1, $2), pos() }
| postfix_expression DOT record_label                          { `Projection ($1, $3), pos() }

arg_spec:
| LPAREN RPAREN                                                { [] }
| LPAREN exps RPAREN                                           { $2 }

exps:
| exp COMMA exps                                               { $1 :: $3 }
| exp                                                          { [$1] }

unary_expression:
| MINUS unary_expression                                       { `UnaryAppl (([], `Minus),      $2), pos() }
| MINUSDOT unary_expression                                    { `UnaryAppl (([], `FloatMinus), $2), pos() }
| PREFIXOP unary_expression                                    { `UnaryAppl (([], `Name $1), $2), pos() }
| postfix_expression                                           { $1 }
| constructor_expression                                       { $1 }

infixr_9:
| unary_expression                                             { $1 }
| unary_expression INFIX9 unary_expression                     { `InfixAppl (([], `Name $2), $1, $3), pos() }
| unary_expression INFIXR9 infixr_9                            { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_9:
| infixr_9                                                     { $1 }
| infixl_9 INFIXL9 infixr_9                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_8:
| infixl_9                                                     { $1 }
| infixl_9 INFIX8  infixl_9                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_9 INFIXR8 infixr_8                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_9 COLONCOLON infixr_8                                 { `InfixAppl (([], `Cons), $1, $3), pos() }

infixl_8:
| infixr_8                                                     { $1 }
| infixl_8 INFIXL8 infixr_8                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_7:
| infixl_8                                                     { $1 }
| infixl_8 INFIX7  infixl_8                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_8 INFIXR7 infixr_7                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_7:
| infixr_7                                                     { $1 }
| infixl_7 INFIXL7 infixr_7                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_6:
| infixl_7                                                     { $1 }
| infixl_7 INFIX6  infixl_7                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_7 INFIXR6 infixr_6                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_6:
| infixr_6                                                     { $1 }
| infixl_6 INFIXL6 infixr_6                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_6 MINUS infixr_6                                      { `InfixAppl (([], `Minus), $1, $3), pos() }
| infixl_6 MINUSDOT infixr_6                                   { `InfixAppl (([], `FloatMinus), $1, $3), pos() }
/* HACK: the type variables should get inserted later... */
| infixl_6 BANG infixr_6                                       { `InfixAppl (([], `Name "!"), $1, $3), pos() }

infixr_5:
| infixl_6                                                     { $1 }
| infixl_6 INFIX5  infixl_6                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_6 INFIXR5 infixr_5                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_5:
| infixr_5                                                     { $1 }
| infixl_5 INFIXL5 infixr_5                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_4:
| infixl_5                                                     { $1 }
| infixl_5 INFIX4    infixl_5                                  { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_5 INFIXR4   infixr_4                                  { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixr_5 EQUALSTILDE regex                                   { let r, flags = $3 in `InfixAppl (([], `RegexMatch flags), $1, r), pos() }

infixl_4:
| infixr_4                                                     { $1 }
| infixl_4 INFIXL4 infixr_4                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_3:
| infixl_4                                                     { $1 }
| infixl_4 INFIX3  infixl_4                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_4 INFIXR3 infixr_3                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_3:
| infixr_3                                                     { $1 }
| infixl_3 INFIXL3 infixr_3                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_2:
| infixl_3                                                     { $1 }
| infixl_3 INFIX2  infixl_3                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_3 INFIXR2 infixr_2                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_2:
| infixr_2                                                     { $1 }
| infixl_2 INFIXL2 infixr_2                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_1:
| infixl_2                                                     { $1 }
| infixl_2 INFIX1  infixl_2                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_2 INFIXR1 infixr_1                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_1:
| infixr_1                                                     { $1 }
| infixl_1 INFIXL1 infixr_1                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixr_0:
| infixl_1                                                     { $1 }
| infixl_1 INFIX0    infixl_1                                  { `InfixAppl (([], `Name $2), $1, $3), pos() }
| infixl_1 INFIXR0   infixr_0                                  { `InfixAppl (([], `Name $2), $1, $3), pos() }

infixl_0:
| infixr_0                                                     { $1 }
| infixl_0 INFIXL0 infixr_0                                    { `InfixAppl (([], `Name $2), $1, $3), pos() }

logical_expression:
| infixl_0                                                     { $1 }
| logical_expression BARBAR infixl_0                           { `InfixAppl (([], `Or), $1, $3), pos() }
| logical_expression AMPAMP infixl_0                           { `InfixAppl (([], `And), $1, $3), pos() }

typed_expression:
| logical_expression                                           { $1 }
| typed_expression COLON datatype                              { `TypeAnnotation ($1, datatype $3), pos() }
| typed_expression COLON datatype LARROW datatype              { `Upcast ($1, datatype $3, datatype $5), pos() }

db_expression:
| typed_expression                                             { $1 }
| DELETE LPAREN table_generator RPAREN perhaps_where           { let pat, phrase = $3 in `DBDelete (pat, phrase, $5), pos() }
| UPDATE LPAREN table_generator RPAREN
         perhaps_where
         SET LPAREN labeled_exps RPAREN                        { let pat, phrase = $3 in `DBUpdate(pat, phrase, $5, $8), pos() }

/* XML */
xml:
| xml_tree                                                     { $1 }

xmlid:
| VARIABLE                                                     { $1 }

attrs:
| block                                                        { [], Some (`Block $1, pos ()) }
| attr_list                                                    { $1, None }
| attr_list block                                              { $1, Some (`Block $2, pos ()) }

attr_list:
| attr                                                         { [$1] }
| attr_list attr                                               { $2 :: $1 }

attr:
| xmlid EQ LQUOTE attr_val RQUOTE                              { ($1, $4) }
| xmlid EQ LQUOTE RQUOTE                                       { ($1, [(`Constant (`String ""), pos() : Sugartypes.phrase)]) }

attr_val:
| block                                                        { [`Block $1, pos ()] }
| STRING                                                       { [`Constant (`String $1), pos()] }
| block attr_val                                               { (`Block $1, pos ()) :: $2 }
| STRING attr_val                                              { (`Constant (`String $1), pos()) :: $2}

xml_tree:
| LXML SLASHRXML                                               { `Xml ($1, [], None, []), pos() }
| LXML RXML ENDTAG                                             { ensure_match (pos()) $1 $3 (`Xml ($1, [], None, []), pos()) }
| LXML RXML xml_contents_list ENDTAG                           { ensure_match (pos()) $1 $4 (`Xml ($1, [], None, $3), pos()) }
| LXML attrs RXML ENDTAG                                       { ensure_match (pos()) $1 $4 (`Xml ($1, fst $2, snd $2, []), pos()) }
| LXML attrs SLASHRXML                                         { `Xml ($1, fst $2, snd $2, []), pos() }
| LXML attrs RXML xml_contents_list ENDTAG                     { ensure_match (pos()) $1 $5 (`Xml ($1, fst $2, snd $2, $4), pos()) }

xml_contents_list:
| xml_contents                                                 { [$1] }
| xml_contents xml_contents_list                               { $1 :: $2 }

xml_contents:
| block                                                        { `Block $1, pos () }
| formlet_binding                                              { $1 }
| formlet_placement                                            { $1 }
| page_placement                                               { $1 }
| xml_tree                                                     { $1 }
| CDATA                                                        { `TextNode (Utility.xml_unescape $1), pos() }

formlet_binding:
| LBRACE logical_expression RARROW pattern RBRACE              { `FormBinding($2, $4), pos()}

formlet_placement:
| LBRACE logical_expression
         FATRARROW logical_expression RBRACE                   { `FormletPlacement ($2, $4, (`ListLit ([], None), pos())), pos () }
| LBRACE logical_expression
         FATRARROW logical_expression
         WITH logical_expression RBRACE                        { `FormletPlacement ($2, $4, $6), pos () }

page_placement:
| LBRACEBAR exp BARRBRACE                                      { `PagePlacement $2, pos() }

session_expression:
| db_expression                                                { $1 }
| SELECT field_label exp                                       { `Select ($2, $3) , pos() }
| OFFER LPAREN exp RPAREN LBRACE perhaps_cases RBRACE          { `Offer ($3, $6, None) , pos() }

conditional_expression:
| session_expression                                           { $1 }
| IF LPAREN exp RPAREN exp ELSE exp                            { `Conditional ($3, $5, $7), pos() }

cases:
| case                                                         { [$1] }
| case cases                                                   { $1 :: $2 }

case:
| CASE pattern RARROW block_contents                           { $2, (`Block ($4), pos()) }

perhaps_cases:
| /* empty */                                                  { [] }
| cases                                                        { $1 }

case_expression:
| conditional_expression                                       { $1 }
| SWITCH LPAREN exp RPAREN LBRACE perhaps_cases RBRACE         { `Switch ($3, $6, None), pos() }
| RECEIVE LBRACE perhaps_cases RBRACE                          { `Receive ($3, None), pos() }

iteration_expression:
| case_expression                                              { $1 }
| FOR LPAREN perhaps_generators RPAREN
      perhaps_where
      perhaps_orderby
      exp                                                      { `Iteration ($3, $7, $5, $6), pos() }

perhaps_generators:
| /* empty */                                                  { [] }
| generators                                                   { $1 }

generators:
| generator                                                    { [$1] }
| generator COMMA generators                                   { $1 :: $3 }

generator:
| list_generator                                               { `List $1 }
| table_generator                                              { `Table $1 }

list_generator:
| pattern LARROW exp                                           { ($1, $3) }

table_generator:
| pattern LLARROW exp                                          { ($1, $3) }

perhaps_where:
| /* empty */                                                  { None }
| WHERE LPAREN exp RPAREN                                      { Some $3 }

perhaps_orderby:
| /* empty */                                                  { None }
| ORDERBY LPAREN exps RPAREN                                   { Some (make_tuple (pos()) $3) }

escape_expression:
| iteration_expression                                         { $1 }
| ESCAPE var IN postfix_expression                             { `Escape ((fst $2, None, snd $2), $4), pos() }

formlet_expression:
| escape_expression                                            { $1 }
| FORMLET xml YIELDS exp                                       { `Formlet ($2, $4), pos() }
| PAGE xml                                                     { `Page ($2), pos() }

table_expression:
| formlet_expression                                           { $1 }
| TABLE exp WITH datatype perhaps_table_constraints FROM exp   { `TableLit ($2, datatype $4, $5, (`ListLit ([], None),pos()), $7), pos()} 
/* SAND */
| TABLE exp WITH datatype perhaps_table_constraints TABLEKEYS exp FROM exp   { `TableLit ($2, datatype $4, $5, $7, $9), pos()} 

perhaps_table_constraints:
| WHERE table_constraints                                      { $2 }
| /* empty */                                                  { [] }

table_constraints:
| record_label field_constraints                               { [($1, $2)] }
| record_label field_constraints COMMA table_constraints       { ($1, $2) :: $4 }

field_constraints:
| field_constraint                                             { [$1] }
| field_constraint field_constraints                           { $1 :: $2 }

field_constraint:
| READONLY                                                     { `Readonly }
| DEFAULT                                                      { `Default }

perhaps_db_args:
| atomic_expression                                            { Some $1 }
| /* empty */                                                  { None }

perhaps_db_driver:
| atomic_expression perhaps_db_args                            { Some $1, $2 }
| /* empty */                                                  { None, None }

database_expression:
| table_expression                                             { $1 }
| INSERT exp VALUES LPAREN RPAREN exp                          { `DBInsert ($2, [], $6, None), pos() }
| INSERT exp VALUES LPAREN record_labels RPAREN exp            { `DBInsert ($2, $5, $7, None), pos() }
| INSERT exp VALUES
  LBRACKET LPAREN labeled_exps RPAREN RBRACKET                 { `DBInsert ($2,
                                                                            labels $6,
                                                                            (`ListLit ([`RecordLit ($6, None), pos()], None), pos()),
                                                                            None),
                                                                 pos() }
| INSERT exp VALUES LPAREN RPAREN db_expression
  RETURNING VARIABLE                                           { `DBInsert ($2, [], $6, Some (`Constant (`String $8), pos())), pos() }
| INSERT exp VALUES LPAREN record_labels RPAREN db_expression
  RETURNING VARIABLE                                           { `DBInsert ($2, $5, $7, Some (`Constant (`String $9), pos())), pos() }
| INSERT exp VALUES
  LBRACKET LPAREN RPAREN RBRACKET
  RETURNING VARIABLE                                           { `DBInsert ($2,
                                                                            [],
                                                                            (`ListLit ([`RecordLit ([], None), pos()], None), pos()),
                                                                            Some (`Constant (`String $9), pos())),
                                                                 pos() }
| INSERT exp VALUES
  LBRACKET LPAREN labeled_exps RPAREN RBRACKET
  RETURNING VARIABLE                                           { `DBInsert ($2,
                                                                            labels $6,
                                                                            (`ListLit ([`RecordLit ($6, None), pos()], None), pos()),
                                                                            Some (`Constant (`String $10), pos())),
                                                                 pos() }
| DATABASE atomic_expression perhaps_db_driver                 { `DatabaseLit ($2, $3), pos() }

record_labels:
| record_label COMMA record_labels                             { $1 :: $3 }
| record_label                                                 { [$1] }

links_open:
| OPEN QUALIFIEDVARIABLE                                       { `Import $2, pos () }
| OPEN QUALIFIEDMODULE                                         { `Import $2, pos () }
| OPEN CONSTRUCTOR                                             { `Import $2, pos () }

binding:
| VAR pattern EQ exp SEMICOLON                                 { `Val ([], $2, $4, `Unknown, None), pos () }
| exp SEMICOLON                                                { `Exp $1, pos () }
| FUN var arg_lists block                                      { `Fun ((fst $2, None, snd $2), `Unl, ([], ($3, (`Block $4, pos ()))), `Unknown, None), pos () }
| LINFUN var arg_lists block                                   { `Fun ((fst $2, None, snd $2), `Lin, ([], ($3, (`Block $4, pos ()))), `Unknown, None), pos () }
| typedecl SEMICOLON                                           { $1 }
| links_module                                                 { $1 }
| links_open                                                   { $1 }

bindings:
| binding                                                      { [$1] }
| bindings binding                                             { $1 @ [$2] }

moduleblock:
| LBRACE bindings RBRACE                                       { ($2, (`RecordLit ([], None), pos())) }

block:
| LBRACE block_contents RBRACE                                 { $2 }

block_contents:
| bindings exp SEMICOLON                                       { ($1 @ [`Exp $2, pos ()], (`RecordLit ([], None), pos())) }
| bindings exp                                                 { ($1, $2) }
| exp SEMICOLON                                                { ([`Exp $1, pos ()], (`RecordLit ([], None), pos())) }
| exp                                                          { [], $1 }
| perhaps_semi                                                 { ([], (`TupleLit [], pos())) }

perhaps_semi:
| SEMICOLON                                                    {}
| /* empty */                                                  {}

exp:
| database_expression                                          { $1 }

labeled_exps:
| record_label EQ exp                                          { [$1, $3] }
| record_label EQ exp COMMA labeled_exps                       { ($1, $3) :: $5 }

/*
 * Datatype grammar
 */
just_datatype:
| datatype END                                                 { $1 }

datatype:
| mu_datatype                                                  { $1 }
| straight_arrow                                               { $1 }
| squiggly_arrow                                               { $1 }

arrow_prefix:
| LBRACE RBRACE                                                { ([], `Closed) }
| LBRACE efields RBRACE                                        { $2 }

straight_arrow_prefix:
| arrow_prefix                                                 { $1 }
| MINUS nonrec_row_var                                         { ([], $2) }
| MINUS kinded_nonrec_row_var                                  { ([], $2) }

squig_arrow_prefix:
| hear_arrow_prefix                                            { $1 }
| arrow_prefix                                                 { $1 }
| TILDE nonrec_row_var                                         { ([], $2) }
| TILDE kinded_nonrec_row_var                                  { ([], $2) }

hear_arrow_prefix:
| LBRACE COLON datatype COMMA efields RBRACE                   { row_with
                                                                   ("wild", `Present `Unit)
                                                                   (row_with
                                                                      ("hear", `Present $3)
                                                                      $5) }
| LBRACE COLON datatype RBRACE                                 { ([("wild", `Present `Unit);
                                                                   ("hear", `Present $3)],
                                                                  `Closed) }
| LBRACE COLON datatype VBAR nonrec_row_var RBRACE             { ([("wild", `Present `Unit);
                                                                   ("hear", `Present $3)],
                                                                  $5) }
| LBRACE COLON datatype VBAR kinded_nonrec_row_var RBRACE      { ([("wild", `Present `Unit);
                                                                   ("hear", `Present $3)],
                                                                  $5) }

straight_arrow:
| parenthesized_datatypes
  straight_arrow_prefix RARROW datatype                        { `Function ($1, $2, $4) }
| parenthesized_datatypes
  straight_arrow_prefix LOLLI datatype                         { `Lolli ($1, $2, $4) }
| parenthesized_datatypes RARROW datatype                      { `Function ($1,
                                                                               ([], fresh_rigid_row_variable None),
                                                                               $3) }
| parenthesized_datatypes LOLLI datatype                       { `Lolli ($1, ([], fresh_rigid_row_variable None), $3) }

squiggly_arrow:
| parenthesized_datatypes
  squig_arrow_prefix SQUIGRARROW datatype                      { `Function ($1,
                                                                               row_with
                                                                                 ("wild", `Present `Unit)
                                                                                 $2,
                                                                               $4) }
| parenthesized_datatypes
  squig_arrow_prefix SQUIGLOLLI datatype                       { `Lolli ($1,
                                                                            row_with
                                                                              ("wild", `Present `Unit)
                                                                            $2,
                                                                            $4) }
/*| parenthesized_datatypes hear_arrow_prefix
  SQUIGRARROW datatype                                         { `Function ($1, $2, $4) }
*/
| parenthesized_datatypes SQUIGRARROW datatype                 { `Function ($1,
                                                                               ([("wild", `Present `Unit)],
                                                                                 fresh_rigid_row_variable None),
                                                                                $3) }
| parenthesized_datatypes SQUIGLOLLI datatype                  { `Lolli ($1,
                                                                            ([("wild", `Present `Unit)],
                                                                             fresh_rigid_row_variable None),
                                                                            $3) }

mu_datatype:
| MU VARIABLE DOT mu_datatype                                  { `Mu ($2, $4) }
| forall_datatype                                              { $1 }

forall_datatype:
| FORALL varlist DOT datatype                                  { `Forall (List.map fst $2, $4) }
| session_datatype                                             { $1 }

session_datatype:
| BANG datatype DOT datatype                                   { `Output ($2, $4) }
| QUESTION datatype DOT datatype                               { `Input ($2, $4) }
| LBRACKETPLUSBAR row BARPLUSRBRACKET                          { `Select $2 }
| LBRACKETAMPBAR row BARAMPRBRACKET                            { `Choice $2 }
| TILDE datatype                                               { `Dual $2 }
| END                                                          { `End }
| primary_datatype                                             { $1 }

parenthesized_datatypes:
| LPAREN RPAREN                                                { [] }
| LPAREN datatypes RPAREN                                      { $2 }

primary_datatype:
| parenthesized_datatypes                                      { match $1 with
                                                                   | [] -> `Unit
                                                                   | [t] -> t
                                                                   | ts  -> `Tuple ts }
| LPAREN rfields RPAREN                                        { `Record $2 }
| TABLEHANDLE
     LPAREN datatype COMMA datatype COMMA datatype RPAREN      { `Table ($3, $5, $7) }
/* | TABLEHANDLE datatype perhaps_table_constraints               { `Table ($2, $3) } */

| LBRACKETBAR vrow BARRBRACKET                                 { `Variant $2 }
| LBRACKET datatype RBRACKET                                   { `List $2 }
| type_var                                                     { $1 }
| kinded_type_var                                              { $1 }
| CONSTRUCTOR                                                  { match $1 with
                                                                   | "Bool"    -> `Primitive `Bool
                                                                   | "Int"     -> `Primitive `Int
                                                                   | "Char"    -> `Primitive `Char
                                                                   | "Float"   -> `Primitive `Float
                                                                   | "XmlItem" -> `Primitive `XmlItem
                                                                   | "String"  -> `Primitive `String
                                                                   | "Database"-> `DB
                                                                   | t         -> `TypeApplication (t, [])
                                                               }
| CONSTRUCTOR LPAREN type_arg_list RPAREN                      { `TypeApplication ($1, $3) }

type_var:
| VARIABLE                                                     { `TypeVar ($1, None, `Rigid) }
| PERCENTVAR                                                   { `TypeVar ($1, None, `Flexible) }
| UNDERSCORE                                                   { fresh_rigid_type_variable None }
| PERCENT                                                      { fresh_type_variable None }

kinded_type_var:
| type_var subkind                                             { attach_subkind (pos()) ($1, $2) }

type_arg_list:
| type_arg                                                     { [$1] }
| type_arg COMMA type_arg_list                                 { $1 :: $3 }

/* TODO: fix the syntax for type arguments
   (TYPE, ROW, and PRESENCE are no longer tokens...)
*/
type_arg:
| datatype                                                     { `Type $1 }
| TYPE LPAREN datatype RPAREN                                  { `Type $3 }
| ROW LPAREN row RPAREN                                        { `Row $3 }
| PRESENCE LPAREN fieldspec RPAREN                             { `Presence $3 }
| LBRACE row RBRACE                                            { `Row $2 }

vrow:
| vfields                                                      { $1 }
| /* empty */                                                  { [], `Closed }

datatypes:
| datatype                                                     { [$1] }
| datatype COMMA datatypes                                     { $1 :: $3 }

row:
| fields                                                       { $1 }
| /* empty */                                                  { [], `Closed }

fields:
| field                                                        { [$1], `Closed }
| field VBAR row_var                                           { [$1], $3 }
| field VBAR kinded_row_var                                    { [$1], $3 }
| VBAR row_var                                                 { [], $2 }
| VBAR kinded_row_var                                          { [], $2 }
| field COMMA fields                                           { $1 :: fst $3, snd $3 }

field:
| field_label                                                  { $1, `Present `Unit }
| field_label fieldspec                                        { $1, $2 }

field_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }
| STRING                                                       { $1 }
| UINTEGER                                                     { string_of_int $1 }

rfields:
| rfield                                                       { [$1], `Closed }
| rfield VBAR row_var                                          { [$1], $3 }
| rfield VBAR kinded_row_var                                   { [$1], $3 }
| VBAR row_var                                                 { [], $2 }
| VBAR kinded_row_var                                          { [], $2 }
| rfield COMMA rfields                                         { $1 :: fst $3, snd $3 }

rfield:
/* The following sugar is tempting, but it leads to a conflict. Is
   the type (a,b,c) a record with fields a, b, c or a polymorphic tuple
   with type variables a, b, c?
*/
/*| record_label                                                 { $1, `Present `Unit } */
| record_label fieldspec                                       { $1, $2 }

record_label:
| field_label                                                  { $1 }

vfields:
| vfield                                                       { [$1], `Closed }
| row_var                                                      { [], $1 }
| kinded_row_var                                               { [], $1 }
| vfield VBAR vfields                                          { $1 :: fst $3, snd $3 }

vfield:
| CONSTRUCTOR                                                  { $1, `Present `Unit }
| CONSTRUCTOR fieldspec                                        { $1, $2 }

efields:
| efield                                                       { [$1], `Closed }
| efield VBAR nonrec_row_var                                   { [$1], $3 }
| efield VBAR kinded_nonrec_row_var                            { [$1], $3 }
| VBAR nonrec_row_var                                          { [], $2 }
| VBAR kinded_nonrec_row_var                                   { [], $2 }
| efield COMMA efields                                         { $1 :: fst $3, snd $3 }

efield:
| effect_label                                                 { $1, `Present `Unit }
| effect_label fieldspec                                       { $1, $2 }

effect_label:
| CONSTRUCTOR                                                  { $1 }
| VARIABLE                                                     { $1 }

fieldspec:
| COLON datatype                                               { `Present $2 }
| LBRACE COLON datatype RBRACE                                 { `Present $3 }
| MINUS                                                        { `Absent }
| LBRACE MINUS RBRACE                                          { `Absent }
| LBRACE VARIABLE RBRACE                                       { `Var ($2, None, `Rigid) }
| LBRACE PERCENTVAR RBRACE                                     { `Var ($2, None, `Flexible) }
| LBRACE UNDERSCORE RBRACE                                     { fresh_rigid_presence_variable None }
| LBRACE PERCENT RBRACE                                        { fresh_presence_variable None }

nonrec_row_var:
| VARIABLE                                                     { `Open ($1, None, `Rigid) }
| PERCENTVAR                                                   { `Open ($1, None, `Flexible) }
| UNDERSCORE                                                   { fresh_rigid_row_variable None }
| PERCENT                                                      { fresh_row_variable None }

/* FIXME:
 *
 * recursive row vars shouldn't be restricted to vfields.
 */
row_var:
| nonrec_row_var                                               { $1 }
| LPAREN MU VARIABLE DOT vfields RPAREN                        { `Recursive ($3, $5) }

kinded_nonrec_row_var:
| nonrec_row_var subkind                                       { attach_row_subkind (pos()) ($1, $2) }

kinded_row_var:
| row_var subkind                                              { attach_row_subkind (pos()) ($1, $2) }

/*
 * Regular expression grammar
 */
regex:
| SLASH regex_pattern_alternate regex_flags_opt                                  { (`Regex $2, pos()), $3 }
| SLASH regex_flags_opt                                                          { (`Regex (`Simply ""), pos()), $2 }
| SSLASH regex_pattern_alternate SLASH regex_replace regex_flags_opt             { (`Regex (`Replace ($2, $4)), pos()), `RegexReplace :: $5 }

regex_flags_opt:
| SLASH                                                        {[]}
| SLASHFLAGS                                                   {parseRegexFlags $1}

regex_replace:
| /* empty */                                                  { `Literal ""}
| REGEXREPL                                                    { `Literal $1}
| block                                                        { `Splice (`Block $1, pos ()) }

regex_pattern:
| RANGE                                                        { `Range $1 }
| STRING                                                       { `Simply $1 }
| QUOTEDMETA                                                   { `Quote (`Simply $1) }
| DOT                                                          { `Any }
| CARET                                                        { `StartAnchor }
| DOLLAR                                                       { `EndAnchor }
| LPAREN regex_pattern_alternate RPAREN                        { `Group $2 }
| regex_pattern STAR                                           { `Repeat (Regex.Star, $1) }
| regex_pattern PLUS                                           { `Repeat (Regex.Plus, $1) }
| regex_pattern QUESTION                                       { `Repeat (Regex.Question, $1) }
| block                                                        { `Splice (`Block $1, pos ()) }

regex_pattern_alternate:
| regex_pattern_sequence                                       { `Seq $1 }
| regex_pattern_sequence ALTERNATE regex_pattern_alternate     { `Alternate (`Seq $1, $3) }

regex_pattern_sequence:
| regex_pattern                                                { [$1] }
| regex_pattern regex_pattern_sequence                         { $1 :: $2 }

/*
 * Pattern grammar
 */
pattern:
| typed_pattern                                             { $1 }
| typed_pattern COLON primary_datatype                      { (`HasType ($1, datatype $3), pos()) }

typed_pattern:
| cons_pattern                                              { $1 }
| cons_pattern AS var                                       { `As ((fst $3, None, snd $3), $1), pos() }

cons_pattern:
| constructor_pattern                                       { $1 }
| constructor_pattern COLONCOLON cons_pattern               { `Cons ($1, $3), pos() }

constructor_pattern:
| negative_pattern                                          { $1 }
| CONSTRUCTOR                                               { `Variant ($1, None), pos() }
| CONSTRUCTOR parenthesized_pattern                         { `Variant ($1, Some $2), pos() }

constructors:
| CONSTRUCTOR                                               { [$1] }
| CONSTRUCTOR COMMA constructors                            { $1 :: $3 }

negative_pattern:
| primary_pattern                                           { $1 }
| MINUS CONSTRUCTOR                                         { `Negative [$2], pos() }
| MINUS LPAREN constructors RPAREN                          { `Negative $3, pos() }

parenthesized_pattern:
| LPAREN RPAREN                                             { `Tuple [], pos() }
| LPAREN pattern RPAREN                                     { $2 }
| LPAREN pattern COMMA patterns RPAREN                      { `Tuple ($2 :: $4), pos() }
| LPAREN labeled_patterns VBAR pattern RPAREN               { `Record ($2, Some $4), pos() }
| LPAREN labeled_patterns RPAREN                            { `Record ($2, None), pos() }

primary_pattern:
| VARIABLE                                                  { `Variable ($1, None, pos()), pos() }
| UNDERSCORE                                                { `Any, pos() }
| constant                                                  { let c, p = $1 in `Constant c, p }
| LBRACKET RBRACKET                                         { `Nil, pos() }
| LBRACKET patterns RBRACKET                                { `List $2, pos() }
| parenthesized_pattern                                     { $1 }

patterns:
| pattern                                                   { [$1] }
| pattern COMMA patterns                                    { $1 :: $3 }

labeled_patterns:
| record_label EQ pattern                                   { [($1, $3)] }
| record_label EQ pattern COMMA labeled_patterns            { ($1, $3) :: $5 }

multi_args:
| LPAREN patterns RPAREN                                    { $2 }
| LPAREN RPAREN                                             { [] }

arg_lists:
| multi_args                                                { [$1] }
| multi_args arg_lists                                      { $1 :: $2 }
