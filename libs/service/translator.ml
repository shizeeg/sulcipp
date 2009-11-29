
(*                                                                          *)
(* (c) 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Xml
open Common
open Error

let ext = ".htbl"
let lang_htbls:(string, (string, string) Hashtbl.t) Hashtbl.t = 
   Hashtbl.create 5

let load dir lang =
   let htbl =  Marshal.from_channel 
      (open_in_bin (Filename.concat dir (lang ^ ext))) in
      Hashtbl.add lang_htbls lang htbl;
      htbl

(*
let process str args =
   let rec cycle part arges =
      if arges = [] then part
      else
         try
            let mark = String.index part '%' in
               if part.[mark+1] = 's' then
                  (String.sub part 0 mark) ^ (List.hd arges) ^ 
                     cycle (string_after part (mark+2)) (List.tl arges)
               else
                  String.sub part 0 (mark+2) ^
                     (cycle (string_after part (mark+2)) arges)
         with Not_found ->
            part
      in
         cycle str args
*)
(*
let get_msg_s lang msgid args =
   let lang = select_lang lang in
   let htbl = LangMap.find lang !langmsgs in
   let str =  Hashtbl.find htbl msgid in
      lang, process str args

let get_msg ?lang ?lang_htbl msgid =
   let htbl = 
      match lang with
	 | None -> 
	      (match lang_htbl with
		  | None ->
		       LangMap.find !deflang !langmsgs
		  | Some htbl ->
		       htbl)
	 | Some l ->
	      LangMap.find l !langmsgs
   in
   let str = 
      try Hashtbl.find htbl msgid with _ ->
	 try
            let hashtbl = LangMap.find !deflang !langmsgs in
               Hashtbl.find hashtbl msgid
	 with Not_found ->
	    LOG WARN "Not found in lang pack: %s" msgid END;
	    "[not found in lang pack"
   in
      str

*)
(*
let msg_error msg xml error =
   let lang = get_lang xml in
      make_error_reply xml error ~text:(get_msg ~lang msg) ~text_lang:lang
*)

let prepare_lang dir deflang =
   let defhtbl = 
      try load dir deflang with
	 | Sys_error txt ->
	      Printf.eprintf "Cannot load lang pack: %s\n" txt;
	      Pervasives.exit 127
	 | exn ->
	      raise exn
   in
   let rec select_lang lang =
      if Hashtbl.mem lang_htbls lang then
	 lang
      else
	 try 
	    let _ = load dir lang in
	       lang
	 with _ -> 
	    try
	       let hyphen = String.index lang '-' in
	       let lang' = String.sub lang 0 hyphen in
		  select_lang lang'
	    with _ ->
	       deflang
   in
   let get_htbl lang = 
      try Hashtbl.find lang_htbls lang with Not_found ->
	 try load dir lang with Not_found -> defhtbl 
   in
      fun ?lang ?xml msgids ->
	 let htbl = 
	    match lang with
	       | None -> 
		    (match xml with
			| None ->
			     defhtbl
			| Some x ->
			     try get_htbl (select_lang 
					      (get_attr_s x "xml:lang"))
			     with Not_found -> defhtbl)
	       | Some l ->
		    get_htbl l
	 in
	    List.map (fun msgid ->
			 let str = try Hashtbl.find htbl msgid with _ ->
			    try Hashtbl.find defhtbl msgid with _ -> msgid in
			    (msgid, str)) msgids
			       
