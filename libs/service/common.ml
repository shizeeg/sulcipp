(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml

let string_after s n =
  String.sub s n (String.length s - n)

let skip_ws str =
   if str = "" then str
   else
      let rec cycle i =
	 if i = String.length str then ""
	 else
	    if List.mem str.[i] [' '; '\n'; '\r'; '\t'] then cycle (succ i)
	    else if i > 0 then string_after str i
	    else str
      in
	 cycle 0

let rskip_ws str =
   if str = "" then str
   else
      let rec cycle i =
	 if i = -1 then ""
	 else
	    if List.mem str.[i] [' '; '\n'; '\t'; '\r'] then cycle (pred i)
	    else String.sub str 0 (i+1)
      in
	 cycle (pred (String.length str))

let trim str =
   let r1 = skip_ws str in
      rskip_ws r1
