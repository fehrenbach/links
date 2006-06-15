(*
  Compiler settings
*)
module SettingsMap = Utility.StringMap

type 'a setting = {mutable value: 'a; name: string; user: bool}
(*
[SUGGESTION]
  add an optional function to each setting
  that is called whenever the setting is updated 
*)
(* ('a * 'a -> unit) option*)

let get_value setting = setting.value
let get_name setting = setting.name

let set_value setting v = setting.value <- v

type universal = [
| `Bool of bool setting
| `Int of int setting
| `String  of string setting
]

let parse_bool = function
  | "true"
  | "yes"
  | "oui"
  | "ja"
  | "on"  -> true
  | "false"
  | "no"
  | "non"
  | "nein"
  | "off" -> false

let is_user_setting = function
  | `Bool setting -> setting.user
  | `Int setting -> setting.user
  | `String setting -> setting.user

let settings : ((universal SettingsMap.t) ref) = ref (SettingsMap.empty)

(* parse and set *)
let parse_and_set' : bool -> (string * string) -> unit = fun user_check (name, value) ->
  if SettingsMap.mem name (!settings) then
    let universal_setting = SettingsMap.find name (!settings) in
      if (user_check && is_user_setting (universal_setting)) then
	match universal_setting with
	  | `Bool setting ->
	      begin
		try
		  set_value setting (parse_bool value)
		with (Invalid_argument _) ->
		  output_string stderr ("Setting '" ^ name ^ "' expects a boolean\n"); flush stderr
	      end
	  | `Int setting ->
	      begin
		try
		  set_value setting (int_of_string value)
		with Invalid_argument _ ->
		  output_string stderr ("Setting '" ^ name ^ "' expects a boolean\n"); flush stderr
	      end
	  | `String setting ->
	      set_value setting value
      else
	output_string stderr ("Cannot change system setting '" ^ name ^ "'\n"); flush stderr;
  else
     output_string stderr ("Unknown setting: " ^ name ^ "\n"); flush stderr

let parse_and_set = parse_and_set' false     (* any setting can be set *)
let parse_and_set_user = parse_and_set' true (* only allow user settings to be set *)


let add : string -> universal -> unit = fun name universal_setting ->
  if SettingsMap.mem name (!settings) then
    failwith ("Setting: "^name^" already present")
  else
    (settings := SettingsMap.add name universal_setting (!settings))

let add_bool (name, value, user) =
  let setting = {value=value; name=name; user=user} in
    add name (`Bool setting);
    setting
let add_int (name, value, user) =
  let setting : int setting = {value=value; name=name; user=user} in
    add name (`Int setting);
    setting
let add_string (name, value, user) =
  let setting = {value=value; name=name; user=user} in
    add name (`String setting);
    setting

let lookup name =
  (assert(SettingsMap.mem name (!settings));
   SettingsMap.find name (!settings))

let lookup_bool name =
  match lookup name with
    | `Bool setting -> setting
    | _ -> assert(false)
let lookup_int name =
  match lookup name with
    | `Int setting -> setting
    | _ -> assert(false)
let lookup_string name =
  match lookup name with
    | `String setting -> setting
    | _ -> assert(false)


let format_universal formatter : universal -> string = fun universal_setting ->
  match universal_setting with
    | `Bool setting ->
	formatter setting.name (string_of_bool setting.value)
    | `Int setting ->
	formatter setting.name (string_of_int setting.value)
    | `String setting ->
	formatter setting.name setting.value

let print_settings () = 
  let get_settings user =
    List.rev
      (SettingsMap.fold (fun _ setting p ->
			   if (user && (is_user_setting(setting)))
			     || (not(user) && not(is_user_setting(setting))) then
			       (format_universal
				  (Printf.sprintf " %-25s %-7s") setting)::p
			   else
			     p) !settings []) in
  let user_settings =
    ("User settings" :: get_settings true) in
  let system_settings =
    ("System settings" :: get_settings false)
  in
    [""] @ user_settings @ [""] @ system_settings
    