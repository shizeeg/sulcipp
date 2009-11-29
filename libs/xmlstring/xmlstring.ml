(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml

let regexp bom = 0xFEFF
(* let regexp bom = 239 187 191 *)

let regexp space = ' ' | '\n' | '\t' | '\r'

let regexp any = [0x9 0xA  0xD 0x20-0xD7FF 0xE000-0xFFFD 0x10000-0x10FFFF]

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

let convert_digital_entity e =
   let i = int_of_string e in
      if i = 0x9 || i = 0xA  || i =0xD || 
	 (i >= 0x20 && i <= 0xD7FF) ||
	 (i >= 0xE000 && i <= 0xFFFD) ||
	 (i >= 0x10000 && i <= 0x10FFFF) then
	    let b = Buffer.create 4 in
	       Utf8.store b i;
	       Buffer.contents b
      else
	 raise (XmlError (e, "Inadmissible symbol for XML Char"))

let validate_digital_entity e =
   let substr = String.sub e 2 (String.length e - 3) in
   let i = if substr.[0] = 'x' || substr.[0] = 'X' then
      int_of_string ("0" ^ substr) else int_of_string substr in
      if i = 0x9 || i = 0xA  || i =0xD || 
	 (i >= 0x20 && i <= 0xD7FF) ||
	 (i >= 0xE000 && i <= 0xFFFD) ||
	 (i >= 0x10000 && i <= 0x10FFFF) then
	    ()
      else
	 raise (XmlError (e, "Inadmissible symbol for XML Char"))
	 
let rec token = lexer
   | bom? "<?xml" ->
	let _ = xmldeclattrs [] lexbuf in
	   token lexbuf
   | "<?xml-" ->
	let _ = pi lexbuf in
	   token lexbuf
   | "<?" ->
        let _ = pi lexbuf in
           token lexbuf

   | "<!--" -> 
	comment lexbuf;
	token lexbuf
   | "<!DOCTYPE" ->
	doctype lexbuf;
	token lexbuf
   | '<' ->
        let tag = name lexbuf in
        let empty, attrs = attributes [] lexbuf in
        let subels = match empty with
           | true -> 
		[]
           | false -> 
		elements tag [] lexbuf
        in
           Xmlelement (tag, attrs, subels)
   | space+ -> token lexbuf
   | eof ->
	parse_error "" "Unexpected end"
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML token"

and pi = lexer
   | "?>" -> ""
   | any -> pi lexbuf

and xmldeclattrs acc = lexer
   | space* "?>" space* ->
	acc
   | space+ ->
	let attr = attribute lexbuf in
	   xmldeclattrs (attr :: acc) lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [xmldecl]"

and comment = lexer
   | [^'-']* -> comment lexbuf
   | "-->" -> ""
   | "--" -> parse_error "--" "'--' not allowed inside comments"
   | "-" -> comment lexbuf
   | any -> parse_error (Ulexing.utf8_lexeme lexbuf) "Expecting comment end"

and doctype =  lexer
   | '>' -> ""
   | '[' [^"]"]* ']' -> doctype lexbuf  
   | any -> doctype lexbuf

and element = lexer
   | name ->
	let tag = Ulexing.utf8_lexeme lexbuf in
	let empty, attrs = attributes [] lexbuf in
	   if empty then
	      Xmlelement (tag, attrs, [])
	   else
	      Xmlelement (tag, attrs, (elements tag [] lexbuf))
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [element]"

and elements tag acc = lexer
   | "<!--" -> 
	let _ = comment lexbuf in
	   elements tag acc lexbuf
   | "</" ->
	let endtag = name lexbuf in
	   if endtag = tag then 
	      begin
		 eat_end lexbuf;
		 List.rev acc
	      end
	   else
	      parse_error endtag ("Invalid XML end tag: expected " ^ tag)
   | '<' ->
	elements tag ((element lexbuf) :: acc) lexbuf
   | [^'<'] ->
	Ulexing.rollback lexbuf;
        let cdatatxt = cdata [] lexbuf in
	   elements tag ((Xmlcdata cdatatxt) :: acc) lexbuf
   | "<![CDATA[" ->
        let spec_cdata = special_cdata [] lexbuf in
           elements tag ((Xmlcdata spec_cdata) :: acc) lexbuf
   | any -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [elements]"

and attributes attrs = lexer
   | space* "/>" -> 
	true, attrs
   | space* '>' -> 
	false, attrs
   | space+ ->
	let attr = attribute lexbuf in
           attributes (attr :: attrs) lexbuf
   | _ ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Bad attribute line"

and name = lexer
   | name -> 
	Ulexing.utf8_lexeme lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid char in tag"

and attribute = lexer
   | name '=' ->
	let attrname = enclosed lexbuf 0 1 in
           attrname, attvalue lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid attribute name"

and attvalue = lexer
   | "\"" -> 
	attvalue_quot [] lexbuf
   | "'" -> 
	attvalue_apos [] lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "No quotes"

and attvalue_quot acc = lexer
   | ent ->
	attvalue_quot (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | "&#" ->
	attvalue_quot (unicode lexbuf :: acc) lexbuf
   | '&' ->
	parse_error "&" "Not expected in attribute value"
   | '<' ->
	parse_error "<" "Not expected in attribute value"
   | "\"" -> 
	String.concat "" (List.rev acc)
   | any ->
	attvalue_quot (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) 
	   "Not expected in attribute value"

and attvalue_apos acc = lexer
   | ent ->
	attvalue_apos (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | "&#" ->
	attvalue_apos (unicode lexbuf :: acc) lexbuf
   | '&' ->
	parse_error "&" "Not expected in attribute value"
   | '<' ->
	parse_error "<" "Not expected in attribute value"
   | "'" -> 
	String.concat "" (List.rev acc)
   | any ->
	attvalue_apos (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) 
	   "Not expected in attribute value"

and cdata (acc:string list) = lexer
   | ent ->
	cdata (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | "&#" ->
	cdata (unicode lexbuf :: acc) lexbuf
   | '<' -> 
	Ulexing.rollback lexbuf;
	String.concat "" (List.rev acc)
   | "&" ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML CDATA"
   | any ->
	cdata (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | _ ->
	parse_error (Ulexing.utf8_lexeme lexbuf) 
	   "Char is not in XML Chars range"

and unicode = lexer
   | ['0'-'9']+ ';' ->
	let l = enclosed lexbuf 0 1 in
	   convert_digital_entity l
   | 'x' ['0'-'0' 'a'-'f' 'A'-'F']+ ';' ->
	let l = "0" ^ enclosed lexbuf 0 1 in
	   convert_digital_entity l
   | _ ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "invalid unicode"

and eat_end = lexer
   | space* '>' ->
	()
   | any ->
	parse_error (Ulexing.utf8_lexeme lexbuf) "Invalid XML [get_end]"

and special_cdata (acc:string list) = lexer
   | ent ->
	special_cdata (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | "&#" (['0'-'9']+ | ('x'|'X' ['0'-'9''a'-'f''A'-'F']+)) ';' ->
	let e = Ulexing.utf8_lexeme lexbuf in
	   validate_digital_entity e;
	   special_cdata (e :: acc) lexbuf
   | "]]>" -> 
	String.concat "" (List.rev acc)
   | any -> 
	special_cdata (Ulexing.utf8_lexeme lexbuf :: acc) lexbuf
   | _ -> parse_error (Ulexing.utf8_lexeme lexbuf) 
	"Char is not in XML Chars range"

(*
let rec decode_xml acc = lexer
   | [^'&']+ ->
	decode_xml (acc ^ Ulexing.utf8_lexeme lexbuf) lexbuf
   | "&amp;" ->
	decode_xml (acc ^ "&") lexbuf
   | "&quot;" ->
	decode_xml (acc ^ "\"") lexbuf
   | "&lt;" ->
	decode_xml (acc ^ "<") lexbuf
   | "&gt;" ->
	decode_xml (acc ^ ">") lexbuf
   | "&apos;" ->
	decode_xml (acc ^ "'") lexbuf
   | eof ->
	acc
   | _ -> 
	parse_error (Ulexing.utf8_lexeme lexbuf) "unexpected symbol"
*)

let parse_string string =
   let lexbuf = Ulexing.from_utf8_string string in
      try 
	 token lexbuf
      with exn ->
	 raise exn

let parse_from_file file =
   let f = open_in file in
   let lexbuf = Ulexing.from_utf8_channel f in
      try
         token lexbuf
      with exn ->
         raise exn
