(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Netconversion

let regexp bom = 0xFEFF
(* let regexp bom = 239 187 191 *)

let regexp space = ' ' | '\n' | '\t' | '\r'

let regexp any = xml_letter | xml_digit | xml_extender | xml_base_char 
               | xml_ideographic | xml_combining_char | xml_blank

let regexp name_char = xml_letter | xml_digit | '.' | '-' | '_' | ':' 
                     | xml_combining_char | xml_extender
let regexp name = (xml_letter | '_' | ':') (name_char* )

let regexp ent = "&lt;" | "&gt;" | "&amp;" | "&quot;" | "&apos;"

exception XmlError of string * string

let enclosed lexbuf offset_begin offset_end =
   let len = Ulexing.lexeme_length lexbuf in
      Ulexing.utf8_sub_lexeme lexbuf offset_begin
         (len - (offset_end + offset_begin))

let parse_error buf str =
   raise (XmlError (buf, str))

let rec  name = lexer
   | name -> 
	Ulexing.utf8_lexeme lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid char in tag"

let rec attribute = lexer
   | name '=' ->
	let attrname = enclosed lexbuf 0 1 in
           attrname, attvalue lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid attribute name"

and attvalue = lexer
   | "\"" -> 
	attvalue_quot "" lexbuf
   | "'" -> 
	attvalue_apos "" lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "No quotes"

and attvalue_quot value = lexer
   | ([^ '<' '&' '\"' ] | ent)* ->
	attvalue_quot (value ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf
   | "\"" -> 
	value
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Not expected here"

and attvalue_apos value = lexer
   | ([^ '<' '&' '\''] | ent)* ->
	attvalue_apos (value ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf
   | "'" -> 
	value
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Not expected here"

let rec xmldecl = lexer
   | bom? "<?xml" ->
	xmldeclattrs [] lexbuf
   | _ ->
	Ulexing.rollback lexbuf;
	[]

and xmldeclattrs acc = lexer
   | space* "?>" space* ->
	acc
   | space+ ->
	let attr = attribute lexbuf in
	   xmldeclattrs (attr :: acc) lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [xmldecl]"

let parse_string string =
   let enc = ref Ulexing.Latin1 in
   let lexbuf = Ulexing.from_var_enc_string enc string in
   try
      let xmlattrs = xmldecl lexbuf in
	 let xml_enc = try List.assoc "encoding" xmlattrs with _ -> "UTF-8" in
	 let rest = 
	    let l =  Ulexing.lexeme_end lexbuf in
	       String.sub string l (String.length string - l) in

	 let newstr = 
	    if xml_enc = "UTF-8" then
	       rest 
	    else
	       let enc = encoding_of_string xml_enc in
		  convert ~in_enc:enc ~out_enc:`Enc_utf8 rest
	 in
	 let lexbuf = Ulexing.from_utf8_string newstr in
	    Xmlstring.token lexbuf
   with exn ->
(*
      Printf.printf "%s: [%s]\n" (Printexc.to_string exn) 
      (String.sub string 0 (Ulexing.lexeme_end lexbuf));
      flush Pervasives.stdout;
*)
      raise exn
