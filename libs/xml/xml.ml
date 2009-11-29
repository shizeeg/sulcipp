(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

exception NonXmlelement
exception Expected of string

type element =
   | Xmlelement of string * (string * string) list * element list
   | Xmlcdata of string

(*
let crypt s =
   let s1 = Str.global_replace (Str.regexp "&") "&amp;" s in
   let s2 = Str.global_replace (Str.regexp "<") "&lt;" s1 in
   let s3 = Str.global_replace (Str.regexp ">") "&gt;" s2 in
   let s4 = Str.global_replace (Str.regexp "\"") "&quot;" s3 in
   let s5 = Str.global_replace (Str.regexp "'")  "&apos;" s4 in
      s5
*)

let rec attrs_to_string attrs =
   let attr_to_string attr =
      match attr with
         | (name, value) -> 
	      Printf.sprintf " %s='%s'" name value 
		 (* (crypt value) *)
   in List.fold_left (^) "" (List.map attr_to_string attrs)

let rec element_to_string el =
   match el with
      | Xmlelement (name, attrs, els) ->
           if List.length els > 0 then
              (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ ">" ^
              (List.fold_left (^) "" (List.map element_to_string els)) ^
              (Printf.sprintf "</%s>" name)
           else
              (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ "/>"
      | Xmlcdata (chunk) -> chunk (* crypt chunk *)

let rec get_tag (el:element) (path:string list) =
   match el with
      | Xmlelement (_,_, els) ->
           if path = [] then el
           else
              let name = List.hd path in
              let ctag = List.find
                 (function
                     | Xmlelement (name1, _,_) when name1 = name -> 
			  true
                     | _ -> 
			  false
                 ) els in
                 get_tag ctag (List.tl path)
      | Xmlcdata _ -> raise NonXmlelement

let get_tag_full_path el path =
   match el with
      | Xmlelement (tag, _,_) ->
           if tag = List.hd path then get_tag el (List.tl path)
           else raise Not_found
      | Xmlcdata cdata -> 
	   raise NonXmlelement

let get_subel ?(path=[]) el =
   match get_tag el path with
      | Xmlelement (_, _, els) ->
	   List.find (function
			 | Xmlelement (_, _, _) -> true
			 | _ -> false
		     ) els
      | _ -> raise NonXmlelement

let get_subels ?(path=[]) ?(tag="") el =
   match get_tag el path with
      | Xmlelement (_, _, els) ->
           if tag = "" then els
	   else if els = [] then []
	   else
              List.find_all (function x ->
                                match x with
                                   | Xmlelement (tag1, _,_) when tag1=tag -> 
					true
                                   | _ -> false
                            ) els
      | _ -> 
	   raise NonXmlelement

let get_attr_s el ?(path=[]) (attrname:string) =
   match get_tag el path with
      | Xmlelement (_, attrs, _) ->
           List.assoc attrname attrs
      | _ -> raise NonXmlelement

let filter_attrs attrs =
   let checker (k,v) = if v = "" then false else true in
      List.filter checker attrs

let rec collect_cdata els acc =
   match els with
      | (Xmlcdata cdata) :: l -> collect_cdata l (cdata :: acc)
      | _ :: l -> collect_cdata l acc
      | [] -> String.concat "" (List.rev acc)

let get_cdata ?(path=[]) el =
   match get_tag el path with
      | Xmlelement (_, _, els) -> collect_cdata els []
      | _ -> raise NonXmlelement

let make_element name attrs els =
   Xmlelement (name, attrs, els)

let make_simple_cdata name cdata =
   Xmlelement (name, [], [Xmlcdata cdata])

let safe_get_attr_s xml ?(path=[]) attrname =
   try get_attr_s xml ~path attrname with _ -> ""

let match_tag tag element =
   match element with
      | Xmlelement (tag1, _, _) when tag1 = tag -> ();
      | _ ->
	   raise (Expected tag)

let exists_element tag els =
   List.exists (function
		   | Xmlelement (tag1, _, _) when tag1 = tag ->
			true
		   | _ ->
			false
	       ) els


let find_subtag (subels:element list) (tag:string) =
   List.find (function
                 | Xmlelement (tag1, _, _) when tag1=tag -> true
                 | _ -> false
             ) subels

let get_tagname el =
   match el with
      | Xmlelement (name, _, _) -> name
      | _ -> raise NonXmlelement

let match_xml el tag (attrs:(string * string) list) =
   match el with
      | Xmlelement (name, _, _) when name = tag ->
	   (try
               List.iter (fun (a, v) ->
			     if get_attr_s el a <> v then 
				raise Not_found) attrs;
               true
	    with _ -> false)
      | _ -> false

let mem_xml xml path tag attrs =
   if get_tagname xml <> List.hd path then false
   else
      try
	 let els = get_subels xml ~path:(List.tl path) ~tag in
	    List.exists (fun el ->
			    try
			       List.iter (fun (a, v) -> 
					     if get_attr_s el a <> v 
					     then raise Not_found) attrs;
			       true
			    with _ -> false
			) els
      with _ -> false

let get_by_xmlns xml ?path ?tag xmlns =
   let els = get_subels xml ?path ?tag in
      List.find (fun x ->
		      if safe_get_attr_s x "xmlns" = xmlns then true
		      else false) els

let decode = Xml_decode.decode
let encode = Xml_encode.encode
