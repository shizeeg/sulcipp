(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xml
open Xmlstring
open Getopt

let config version program_name =
   let version arg = 
      Printf.printf 
	 "%s %s\n" program_name version;
      flush Pervasives.stdout;
      Pervasives.exit 0
   in

   let configfile = ref (
      let myname = try
	 let r = String.rindex Sys.argv.(0) '/' in
	    String.sub Sys.argv.(0) (r+1) (String.length Sys.argv.(0) - r-1)
      with Not_found -> Sys.argv.(0) in
	 "./" ^ myname ^ ".conf") in

   let opts = ["v", "version", OptEmpty, "Show version", Some version;
	       "c", "config", OptString !configfile, "Path to config file", 
	       None] in
   let parsed = Getopt.parse ~help:true opts in
      List.iter (fun (a,b) ->
		    match a with
		       | Shortopt "c"
		       | Longopt "config" ->
			    (match b with
				| OptString str -> configfile := str
				| _ -> ())
		       | _ -> ()
		) parsed;

      if not (Sys.file_exists !configfile) then begin
	 Printf.eprintf "Cannot find a configuration file: %s\n" !configfile;
	 flush Pervasives.stdout;
	 Pervasives.exit 127
      end
      else
	 try
            Xmlstring.parse_from_file !configfile
	 with exn ->
	    Printf.eprintf "Cannot parse config file %s: %s\n"
	       !configfile (Printexc.to_string exn);
	    flush Pervasives.stdout;
	    Pervasives.exit 127
