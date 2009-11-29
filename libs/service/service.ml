(*                                                                          *)
(* (c) 2004, 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>              *)
(*                                                                          *)

open Xmpp
open Xml
open Common

let program_name = "Service Template"
let version = "0.1"

let start_time = Unix.gettimeofday ()

let setup_log cfg =
   try Some (Logger.open_log
		(Xml.get_cdata cfg ~path:["log"; "report"]))
   with Not_found -> None

let setup_translate cfg =
   let deflang = Xml.get_attr_s cfg ~path:["lang"] "default"
   and dir = Xml.get_attr_s cfg ~path:["lang"] "dir" in
      Translator.prepare_lang dir deflang

let rec default_process_xml next_xml out =
   let xml = next_xml () in
      (match xml with
	  | Xmlelement (tag, attrs, subels) ->
	       let newattr = make_attrs_reply attrs in
		  out (Xmlelement (tag, newattr, subels))
	  | _ -> ());
      default_process_xml next_xml out
	      
let start ?process_xml ?onstart ?onquit ?cleanup ?cfg ?log () =
   let cfg = match cfg with
      | None -> Config.config program_name version
      | Some c -> c
   in
   let () =
      try
	 let pidfile = Xml.get_attr_s cfg ~path:["pid"] "file" in            
	    if Sys.file_exists pidfile then begin
	       Printf.eprintf "Is another service running?\n";
	       flush Pervasives.stdout;
	       Pervasives.exit 127
	    end
	    else
	       let fout = open_out pidfile in
		  output_string fout (string_of_int (Unix.getpid ()));
		  flush fout;
		  close_out fout
      with 
	 | Not_found ->
	      ()
	 | exn ->
	      Printf.eprintf "cannot create pidfile: %s\n" 
		 (Printexc.to_string exn);
	      flush Pervasives.stdout;
	      Pervasives.exit 127
   in
   let server = trim (Xml.get_cdata cfg ~path:["jabber"; "server"]) in
   let port = int_of_string (trim (Xml.get_cdata cfg 
                                      ~path:["jabber"; "port"])) in
   let username = trim (Xml.get_cdata cfg ~path:["jabber"; "name"]) in
   let password = trim (Xml.get_cdata cfg
                           ~path:["jabber"; "password"]) in

   let rawxml_log = 
      try Some (Xml.get_cdata cfg ~path:["log"; "rawxml"])
      with Not_found -> None in

   let run () =
      let out, next_xml = service ?rawxml_log server port username password in
	 (match onquit with
	     | None -> ()
	     | Some proc ->
		  Sys.set_signal Sys.sigint
		     (Sys.Signal_handle (function x -> proc out));
		  Sys.set_signal Sys.sigterm
		     (Sys.Signal_handle (function x -> proc out));
	 );
	 LOG INFO log "Connected!" END;
	 (match onstart with
	     | None -> ()
	     | Some proc -> proc out
	 );
	 match process_xml with
	    | None -> default_process_xml next_xml out
	    | Some callback -> callback next_xml out
   in
   let reconnect_interval = 
      try int_of_string (trim (Xml.get_attr_s cfg
                                  ~path:["reconnect"] "interval"))
      with Not_found -> 0
   in
   let count =
      try int_of_string (trim (Xml.get_attr_s cfg
                                  ~path:["reconnect"] "count"))
      with Not_found -> 0
   in
   let rec reconnect times =
      try
         if times >= 0 then
            run ()
         else
            ()
      with
         | Unix.Unix_error (code, "connect", _) ->
	      LOG INFO log "Unable to connect to %s:%d: %s"
                 server port (Unix.error_message code) END;
	      if count = 0 then begin
                 Unix.sleep reconnect_interval;
		 LOG INFO log "Reconnecting." END;
		 reconnect 1
	      end
	      else if times > 0 then begin
                 Unix.sleep reconnect_interval;
		 LOG INFO log "Reconnecting. Attempts remains: %d" times END;
		 reconnect (times -  1)
              end
(*
         | Auth.AuthError ->
	      LOG INFO log "Authorization failed" END;
              Pervasives.exit 127
*)
         | Xmpp.XMPPStreamEnd ->
	      LOG INFO log "The connection to the server is lost" END;
	      (match cleanup with
		  | None -> ()
		  | Some proc -> proc ());
	      reconnect count
         | Xmpp.XMPPStreamError els ->
              (* todo: analyze els *)
	      LOG INFO log
                 "The server reject us because we used invalid protocol." END;
              Pervasives.exit 127
         | exn ->
	      LOG EXC log exn END
   in
      reconnect count
