(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

open Proto

let midge_user_agent = "Midge 0.1"
let max_read = Sys.max_string_length
let max_redirect = 5

exception HttpProtocolMalformed
exception HttpNotImplemented of string
exception HttpError of string
exception HttpNotFound
exception HttpDNSError
exception HttpUnableConnect of string

type httpio = {
   fd: Unix.file_descr;
   mutable rsize: int;
}

let url_encode str =
   let buffer = Buffer.create (String.length str) in
      for i = 0 to String.length str - 1 do
	 match str.[i] with
	    | '0'..'9' 
	    | 'a'..'z'
	    | 'A'..'Z' as c ->
		 Buffer.add_char buffer c
	    | '\n' -> 
		 Buffer.add_string buffer "%0D%0A";
	    | c ->
		 Buffer.add_char buffer '%';
		 let hex = Hex.int_to_hex (Char.code c) in
		    if String.length hex = 0 then
		       Buffer.add_char buffer '0';
		    Buffer.add_string buffer hex;
      done;
      Buffer.contents buffer

let rec syscall f = 
   try f () with Unix.Unix_error (Unix.EINTR, _, _) -> syscall f

let midge_connect server port =
   let inet_addr =
      try
	 try Unix.inet_addr_of_string server 
	 with Failure("inet_addr_of_string") ->
            (Unix.gethostbyname server).Unix.h_addr_list.(0)
      with Not_found ->
	 raise HttpDNSError
   in
   let fd = syscall (fun () -> Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0) in
      try
         syscall (fun () -> Unix.connect fd (Unix.ADDR_INET (inet_addr, port)));
	 {fd = fd; rsize = 0}
      with
	 | Unix.Unix_error (Unix.EINPROGRESS,_,_) ->
	      {fd = fd; rsize = 0 }
	 | Unix.Unix_error (err, _, _) ->
	      raise (HttpUnableConnect (Unix.error_message err))
	 | exn ->
	      raise exn

let midge_close httpio =
   Unix.close httpio.fd

let midge_read httpio len =
   let buf = String.create len in
   let rlen = syscall (fun () -> Unix.read httpio.fd buf 0 len) in
      if rlen = 0 then
	 raise End_of_file
      else begin
	 httpio.rsize <- rlen;
	 String.sub buf 0 rlen
      end

let midge_write httpio data =
   let len = String.length data in
   let rec aux_write pos =
      let n =  syscall (fun () -> Unix.write httpio.fd data pos (len-pos)) in
	 if pos + n = len then () else aux_write (pos + n)
   in
      aux_write 0

let midge_getline httpio =
   let buf = Buffer.create 80 in
   let rec aux_getline () =
      let c = midge_read httpio 1 in
	 Buffer.add_char buf c.[0];
	 if c.[0] = '\n' then begin
	    Buffer.contents buf
	 end
	 else
	    aux_getline ()
   in
      aux_getline ()

let midge_sendline httpio line =
   midge_write httpio (line ^ "\r\n")

let req_to_string mth =
   match mth with
      | GET -> "GET"
      | POST -> "POST"
      | HEAD -> "HEAD"

let http_get_status httpio =
   let line = midge_getline httpio in
      if String.sub line 0 4 <> "HTTP" then
	 raise HttpProtocolMalformed;
      if line.[4] = '/' then
	 if String.sub line 5 3 <> "1.0" && String.sub line 5 3 <> "1.1" then
	    raise HttpProtocolMalformed
	 else
	    if line.[8] = ' ' then
	       try
		  int_of_string (String.sub line 9 3)
	       with _ -> raise HttpProtocolMalformed
	    else
	       raise HttpProtocolMalformed
      else
	 if line.[4] = ' ' then
	    try
	       int_of_string (String.sub line 5 3)
	    with _ -> raise HttpProtocolMalformed
	 else
	    raise HttpProtocolMalformed
	       

let rtrim str =
   let rec aux_rtrim i =
      if i > 0 then
	 if List.mem str.[i] ['\r'; '\n'; '\t'; ' '] then
	    aux_rtrim (i-1)
	 else
	    String.sub str 0 (i+1)
      else
	 ""
   in 
      aux_rtrim (String.length str - 1)

let ltrim str =
   let len = String.length str in
   let rec aux_ltrim i =
      if i < len then
	 if List.mem str.[i] ['\r'; '\n'; '\t'; ' '] then
	    aux_ltrim (i+1)
	 else
	    String.sub str i (String.length str - i)
      else
	 str
   in
      aux_ltrim 0

let rec get_headers httpio (acc:(string * string) list) =
   let line = midge_getline httpio in
   let len = String.length line in
   let header = rtrim line in
      if header = "" then
	 acc
      else
	 let pair = 
	    try 
	       let colon = String.index header ':' in
		  (String.lowercase (rtrim (String.sub header 0 colon)),
		   (ltrim (String.sub header (colon+1) 
		       (String.length header - (colon+1)))))
	    with Not_found ->
	       raise HttpProtocolMalformed
	 in
	    get_headers httpio (pair :: acc)

let read_body httpio len =
   if len <> -1 then
      let rec aux_body_len acc rest =
	 let piece = try midge_read httpio rest with End_of_file -> "" in
	    if piece = "" || 
	       String.length piece + String.length acc = len then
		  acc ^ piece
	    else
	       aux_body_len (acc ^ piece) (rest - String.length piece)
      in
	 aux_body_len "" len
   else
      let rec aux_body acc =
	 let piece = try midge_read httpio (8*1024) with End_of_file -> "" in
	    if piece = "" then acc else aux_body (acc ^ piece)
      in
	 aux_body ""

let rec get_chunks httpio (acc:string) =
   let line = midge_getline httpio in
   let rec check_hex i =
      if List.mem line.[i] ['a'; 'b'; 'c'; 'd'; 'e'; 'f';
                            'A'; 'B'; 'C'; 'D'; 'E'; 'F';
                            '0'; '1'; '2'; '3'; '4'; '5'; 
			    '6'; '7'; '8'; '9'] then
	 check_hex (i+1)
      else i
   in
   let idx = check_hex 0 in
      if idx = 0 then raise (HttpError "Invalid Chunk Size");

      let hex = Hex.hex_to_int (String.sub line 0 idx) in
	 if hex = 0 then acc
	 else
	    let chunked = read_body httpio hex in
	    let _ = midge_getline httpio in
	       get_chunks httpio (acc ^ chunked)

let rec http_request (op:http_method) ?headers ?redirect (url:url) =
   let port = match url.port with | None -> 80 | Some p -> p in
      (* todo: here check if it is ftp *)
   let httpio = midge_connect url.host port in
   let host = 
      if port <> 80 then url.host ^ ":" ^ string_of_int port
      else url.host
   in
      midge_sendline httpio (Printf.sprintf "%s %s HTTP/1.1" (req_to_string op)
			 (match url.path with | None -> "/" | Some p -> p));
      midge_sendline httpio (Printf.sprintf "HOST: %s" host);
      midge_sendline httpio (Printf.sprintf "User-Agent: %s" midge_user_agent);
      begin match headers with
	 | None -> ()
	 | Some hs ->
	      List.iter (fun (h,v) -> midge_sendline httpio (h ^ ": " ^ v)) hs
      end;
      midge_sendline httpio "Connection: close";
      midge_sendline httpio "";

      let status_code = http_get_status httpio in
	 match status_code with
	    | 200
	    | 204
	    | 205
	    | 206
	    | 304 ->
		 let hs = get_headers httpio [] in
		 let transfer_encoding () =
		    try String.lowercase 
		       (List.assoc "transfer-encoding" hs) with Not_found -> ""
		 in
		    begin match transfer_encoding () with
		       | "chunked" ->
			    let body = get_chunks httpio "" in
			       midge_close httpio;
			       body
		       | "" ->
			    let length =
			       if op = HEAD || status_code = 204 || 
				  status_code = 304 then 
				     0
			       else
				  try int_of_string (List.assoc 
							"content-length" hs)
				  with
				     | Not_found -> -1
				     | _ -> 
					  raise (HttpError 
						    ("Invalid Content-Length: "
						     ^
					(List.assoc "content-length" hs)))
			    in
			       if length = 0 then begin
				  midge_close httpio;
				  ""
			       end
			       else
				  let body = read_body httpio length in
				     midge_close httpio;
				     body
		       | _ ->
			    raise (HttpError 
				      ("unknown Transfer-Encoding: " ^ 
					  (List.assoc "transfer-encoding" hs)))
		    end

	    | 301 (* moved *)
	    | 302
	    | 303 ->
		 let r = match redirect with
		    | None -> 0
		    | Some r -> r
		 in
		    if r < max_redirect then
		       let hs = get_headers httpio [] in
		       let location = try List.assoc "location" hs with
			     Not_found -> 
				raise (HttpError "not specified redirect url")
		       in
		       let newurl =
			  if location.[0] = '/' then
			     {url with path = Some location}
			  else
			     Url.parse location
		       in
			  midge_close httpio;
			  http_request op 
			     ?headers:
			     (Some ["HTTP_REFERER",  Url.to_string url])
			     ?redirect:(Some (r+1))
			     newurl;
		    else
		       raise (HttpError "too many redirects")
			  
	    | 401 -> (* need auth *)
		 (*
		  * We already sent out authorization code,
		  * so there's nothing more we can do.
		  *)
		 midge_close httpio;
		 raise (HttpNotImplemented "need auth")

	    | 404 ->
		 midge_close httpio;
		 raise HttpNotFound
		    
	    | 407 -> (* proxy auth *)
		 midge_close httpio;
		 raise (HttpNotImplemented "need auth")
(*
	    | 416 -> (* bad range *)
		 (*
		  * This can happen if we ask for 0 bytes because
		  * we already have the whole file.  Consider this
		  * a success for now, and check sizes later.
		  *)
		 ()
*)
	    | 999 -> (* bad protocol *)
		 midge_close httpio;
		 raise HttpProtocolMalformed

	    | _ -> 
		 midge_close httpio;
		 raise (HttpError ("Status is " ^ string_of_int status_code))

	      
let fetch url_s =
   let url = Url.parse url_s in
      http_request GET url
