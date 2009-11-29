(*
 * (c) 2005-2008 Anastasia Gornostaeva. <ermine@ermine.pp.ru>
 *)

type ini = (string * (string * string) list) list

exception IniError of int

val get_section : ini -> string -> (string * string) list

val get_value : ini -> string -> string -> string

val get_value_list : ?sep:char  -> ini -> string -> string -> string list

val get_value_option : ini -> string -> string -> string option

val get_value_boolean : ini -> string -> string -> bool

val parse : string -> ini

