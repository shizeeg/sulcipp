(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)


type opt_t =
   | Shortopt of string
   | Longopt of string

type optarg_t =
   | OptEmpty
   | OptString of string
   | OptInt of int

let justify_and_print alist =
   let max = ref 0 in
      List.iter (fun (opts, comment, default) ->
		    if String.length opts > !max then max := String.length opts
		) alist;
      let col = !max + 8 in
	 List.iter (fun (opts, comment, default) ->
		       Printf.printf "  %s%s%s\n%s" 
			  opts
			  (String.make (col - (String.length opts)) ' ')
			  comment
			  (match default with
			      | OptEmpty -> ""
			      | OptInt i ->
				   (String.make (col+2) ' ') ^ "Default: " ^
				      string_of_int i ^ "\n"
			      | OptString s ->
				   (String.make (col+2) ' ') ^ "Default: " ^ 
				      s ^ "\n")
		   ) alist
      
(**
optionlist - (short_option, long_option, arg_type:option_t, 
              comment, proc option) list
**)

let parse ?help optionlist =
   let usage () =
      let alist = List.map (fun (a, b, c, d, e) ->
			       "-" ^ a ^ ", --" ^ b ^
				  (match c with
				      | OptEmpty -> ""
				      | OptInt _ -> " <int>"
				      | OptString _ -> " <string>"),
			       d, c
			   ) optionlist
      in
	 print_endline "Options:";
	 let alist1 = 
	    match help with
	       | Some true ->
		    ("-?, -h, --help", 
		     "Display this help", OptEmpty) :: alist
	       | _ ->  alist in
	    justify_and_print alist1
   in
   let invalid_usage () =
      print_endline "Invalid usage.";
      Pervasives.exit 127
   in
   let alen = Array.length Sys.argv in
   let rec find_option opt ol acc =
      match ol with
	 | [] -> 
	      (match help with
		  | None -> 
		       print_endline "Invalid option.";
		       usage ();
		       Pervasives.exit 127
		  | Some true ->
		       (match opt with
			   | Shortopt "h"
			   | Shortopt "?"
			   | Longopt "help" ->
				usage ();
				Pervasives.exit 0
			   | _ -> print_endline "Invalid option.";
				usage ();
				Pervasives.exit 127)
		  | Some false ->
		       print_endline "Invalid option.";
		       usage ();
		       Pervasives.exit 127)
	 | (short, long, arg, comment, proc) :: t ->
	      if (match opt with
		     | Shortopt o -> short = o
		     | Longopt o -> long = o) then
		 (short, long, arg, comment, proc), (acc @ t)
	      else
		 find_option opt t ((short, long, arg, comment, proc)::acc)
   in
   let count_hyphens str =
      let len = String.length str in
      let rec aux_count i =
	 if i < len && str.[i] = '-' then aux_count (i+1)
	 else
	    (i, String.sub str i (len - i))
      in
	 aux_count 0
   in
   let rec aux_parse options i acc =
      if i < alen then
	 let opt =
	    let hyphens, str = count_hyphens Sys.argv.(i) in
	       if hyphens = 1 then
		  Shortopt str
	       else if hyphens = 2 then
		  Longopt str
	       else begin
		  Printf.printf "I dont know what is %s\n" Sys.argv.(i);
		  usage ();
		  Pervasives.exit 127
	       end
	 in
	 let (_, _, arg, _, proc), rest = find_option opt options [] in
	 let arg = match arg with
	    | OptEmpty ->
		 if i+1 < alen then
		    if Sys.argv.(i+1).[0] = '-' then
		       OptEmpty
		    else
		       invalid_usage ()
		 else
		    OptEmpty
	    | OptInt default ->
		 if i+1 < alen then
		    if Sys.argv.(i+1).[0] = '-' then
		       invalid_usage ()
		    else
		       let i = try int_of_string Sys.argv.(i+1) 
		       with _ -> invalid_usage () in
			  OptInt i
		 else
		    invalid_usage ()
	    | OptString default ->
		 if i+1 < alen then
		    if Sys.argv.(i+1).[0] = '-' then
		       invalid_usage ()
		    else
		       OptString Sys.argv.(i+1)
		 else
		    invalid_usage ()
	 in
	    match proc with
	       | Some f -> 
		    f arg;
		    aux_parse rest (i+ (match arg with
					   | OptEmpty -> 1
					   | _ -> 2)) acc
	       | None ->
		    aux_parse rest (i+ (match arg with
					   | OptEmpty -> 1
					   | _ -> 2)) ((opt, arg) :: acc)
      else
	 acc
   in
      aux_parse optionlist 1 []


(*
let _ =
   let version arg = print_endline "Our version is ..."; Pervasives.exit 0 in
   let limit = ref 200 in
   let set_limit arg = 
      match arg with
	 | OptInt i -> limit := i
	 | _ -> ()
   in
   let opts = ["v", "version", OptEmpty, "Show version", 
	       Some version;
	       "c", "config", OptString "./sulci.conf", "Path to config fire", 
	       None;
	       "l", "limit", OptInt 400, "Msg limit", 
	       Some set_limit
	      ] in
   let parsed = parse ~help:true opts in
      print_endline "parsed!";
      Printf.printf "limit %d\n" !limit;
      List.iter (fun (o, v) ->
		    Printf.printf "%s\t\t%s\n"
		       (match o with 
			   | Shortopt str -> "-" ^ str
			   | Longopt str -> "--" ^ str)
		       (match v with
			   | OptEmpty -> "[empty]"
			   | OptInt i -> "(int) " ^ string_of_int i
			   | OptString s -> "(string) " ^ s)
		) parsed

*)
