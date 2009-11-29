(*
 * (c) 2996-2008 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Unix
open Sqlite3
open Sqlite_util

module Ini = Ini_config
  
type cfg = {
  db_file: string;
  db: Sqlite3.db;
  port: int;
  grey_delay: float;
  my_helo: string list;
  my_recipients: string list;
  log: Logger.logger
}

let string_after s n =
  String.sub s n (String.length s - n)

let open_db file =
  let db = db_open file in
  let sql1 =
    "SELECT name FROM SQLITE_MASTER WHERE type='table' AND name='stamps'" in
  let sql2 =
    "CREATE TABLE stamps (triple varchar, stamp float, counter integer);
     CREATE INDEX stamps_idx ON stamps(triple)" in
    create_table file db sql1 sql2;
    db

let check_db cfg triple new_time =
  let tr = escape triple in
  let sql1 = Printf.sprintf
    "SELECT stamp, counter FROM stamps WHERE triple=%s" tr in
  let sql2 = Printf.sprintf
	  "UPDATE stamps SET counter=counter+1 WHERE triple=%s" tr in
  let sql3 = Printf.sprintf
    "INSERT INTO stamps (triple, stamp, counter) VALUES(%s, %s, 0)"
    tr (string_of_float new_time) in
    match get_one_row cfg.db_file cfg.db sql1 with
      | Some r ->
	        if Int64.to_int (int64_of_data r.(1)) < 3 then
            simple_exec cfg.db_file cfg.db sql2;
          float_of_data r.(0), Int64.to_int (int64_of_data r.(1))
      | None ->
          simple_exec cfg.db_file cfg.db sql3;
	        new_time, 0
       
let cleanup_db cfg () =
  let now = gettimeofday () -. 15000. in
    simple_exec cfg.db_file cfg.db
      (Printf.sprintf
        "DELETE FROM stamps WHERE counter=0 AND substr(stamp, 0, 10) < %d"
        (int_of_float now))
        
let rec get_attrs in_chan acc =
  let line = input_line in_chan in
    if line = "" || line.[0] = '\n' || line.[0] = '\r' then 
	    acc
    else
	    let eq = String.index line '=' in
	      get_attrs in_chan 
	        ((String.sub line 0 eq, string_after line (eq+1)) :: acc)
          
let find_attr attr attrs =
  try List.assoc attr attrs with Not_found -> ""

let smtpd_access_policy cfg attrs = 
  let helo = find_attr "helo_name" attrs in
  let recipient = String.lowercase (find_attr "recipient" attrs) in
  let sender = String.lowercase (find_attr "sender" attrs) in
  let client_address = find_attr "client_address" attrs in
    if List.mem helo cfg.my_helo then (
      cfg.log#info "action=reject: HELO %s from %s %s\n"
        helo sender client_address;
	    "reject"
    )
    else if not (List.mem recipient cfg.my_recipients) then (
	    cfg.log#info "action=reject: recipient %s from %s %s\n"
		    recipient sender client_address;
	    "reject"
    )
    else (
	    let triple = client_address ^ "/" ^ sender ^ "/" ^ recipient in
	    let now = time () in
	    let time_stamp, counter = check_db cfg triple now in
	      if counter = 3 then (
	        cfg.log#info "action=OK: %s %s -> %s\n"
		        sender client_address recipient;
	        "OK"
	      )
	      else if now -. time_stamp > cfg.grey_delay then (
	        cfg.log#info "action=dunno: %s %s -> %s\n"
		        sender client_address recipient;
	        "dunno"
	      )         
	      else (
	        cfg.log#info "action=defer_if_permit: %s %s -> %s\n"
		        sender client_address recipient;
	        "defer_if_permit Service is unavailable"
	      )
    )

let rec policy cfg in_chan out_chan =
  let attrs = get_attrs in_chan [] in
    List.iter (fun (a,b) ->
		             cfg.log#debug "%s = %s\n" a b) attrs;
    (match find_attr "request" attrs with
	     | "smtpd_access_policy" ->
		       let reply = smtpd_access_policy cfg attrs in
		         output_string out_chan 
		           (Printf.sprintf "action=%s\n\n" reply);
		         flush out_chan;
	     | _ -> 
		       cfg.log#info "Other request: %s\n"
			       (find_attr "request" attrs);
		       output_string out_chan "\n"
    );
    policy cfg in_chan out_chan
      
let init () =
  let ini =
    try Ini.parse "greybottle.conf"
    with
      | Sys_error msg ->
          Printf.eprintf "%s\n" msg;
          Pervasives.exit 1
      | Ini.IniError msg ->
          Printf.eprintf "Malformed ini config\n";
          Pervasives.exit 1
  in
  let global = Ini.get_section ini "global" in
  let lf = List.assoc "logfile" global in
  let log = new Logger.logger ~max_level:"debug" 
    (* ~destination:(new syslog "local0") () in *)
    ~destination:(new Logger.logfile lf) () in
  let grey_delay = 
	  float_of_string (List.assoc "grey_delay" global) in
  let my_helo = Ini.get_value_list ini "global" "my_helo" in
  let my_recipients = Ini.get_value_list ini "global"  "my_recipients" in
  let file = List.assoc "db_file" global in
  let db = open_db file in
  let port = int_of_string (List.assoc "port" global) in
    {
      db_file = file;
      db = db;
      port = port;
      grey_delay = grey_delay;
      my_helo = my_helo;
      my_recipients = my_recipients;
      log = log
    }

let timerQ = Scheduler.create ()
let _ = Scheduler.run timerQ
        
let get_next_noun () =
  let curr_time = gettimeofday () in
  let curr_tm = localtime curr_time in
  let noun, _ =
    mktime {curr_tm with 
      tm_sec = 0; tm_min = 0; tm_hour = 0;
    tm_mday = curr_tm.tm_mday + 1} in
    noun

let _ =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "-v" then (
    print_endline 
	    "Copyright (c) 2005, Anastasia Gornostaeva, ermine@ermine.pp.ru";
    print_endline "Grey Bottle version 0.1"
  ) else (
    let cfg = init () in
    let inet_addr = inet_addr_any in
    let _ = Scheduler.add_task timerQ (cleanup_db cfg)
	    (gettimeofday () +. 7200.) get_next_noun in
    let () =
	    Sys.set_signal Sys.sigusr1
        (Sys.Signal_handle (fun x -> (cfg.log#get_destination ())#reopen)) in
      cfg.log#info "Starting...\n";
	    let sockaddr = ADDR_INET (inet_addr, cfg.port) in
	      try
	        establish_server (policy cfg) sockaddr;
	      with
	        | Unix_error (code, _, _) ->
		          cfg.log#error "%s\n" (error_message code)
	        | End_of_file ->
		          cfg.log#error "Postfix closed a connection\n"
	        | exn ->
		          cfg.log#error "Exception: %s\n" (Printexc.to_string exn)
  )
    
