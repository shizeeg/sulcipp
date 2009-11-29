(*                                                                          *)
(* (c) 2005 Anastasia Gornostaeva. <ermine@ermine.pp.ru>                    *)
(*                                                                          *)

(*  RFC 1738 *)
{
   open Proto
   exception UrlError
}

rule url_parse = parse
   | ['a'-'z' 'A'-'Z' '0'-'9' '+' '.' '-']+ ':'
	 { let lexeme = Lexing.lexeme lexbuf in
	   let scheme = String.lowercase 
	      (String.sub lexeme 0 (pred (String.length lexeme))) in
	      begin match scheme with
		 | "ftp" ->
		      slashslash lexbuf;
		      let user, pass = userpass lexbuf in
		      let host = host lexbuf in
		      let port = port lexbuf in
		      let path = url_path lexbuf in
			 {protocol = FTP;
			  user = user;
			  password = pass;
			  host = host;
			  port = if port = Some 21 then None else port;
			  path = path;
			  data = ""
			 }
		 | "http"
		 | "https" ->
		      slashslash lexbuf;
		      let user, pass = userpass lexbuf  in
		      let host = host lexbuf in
		      let port = port lexbuf in
		      let path = url_path lexbuf in
			 {protocol = if scheme = "http" then HTTP else HTTPS;
			  user = user;
			  password = pass;
			  host = host;
			  port = 
			       if scheme = "http" && port = Some 80 then None
			       else if scheme = "https" && 
				  port = Some 443 then None
			       else port;
			  path = path;
			  data = ""
			 }
	(*		    
		 | "gopher" ->
		 | "mailto" ->
		 | "news" ->
	*)
		 | "nntp" ->
		      slashslash lexbuf;
		      let host = host lexbuf in
		      let port = port lexbuf in
		      let path = url_path lexbuf in
			 { protocol = NNTP;
			   user = None;
			   password = None;
			   host = host;
			   port = port;
			   path = path;
			   data = ""
			 }
		 | "telnet" ->
		      slashslash lexbuf;
		      let user, pass = userpass lexbuf in
		      let host = host lexbuf in
		      let port = port lexbuf in
		      let path = url_path lexbuf in
			 begin match path  with
			    | Some _ -> 
				 raise (UrllexingError 
					   ("Telnet cannot have any path",
					    Lexing.lexeme_start lexbuf))
			    | None ->
				 { protocol = TELNET;
				   user = user;
				   password = pass;
				   host = host;
				   port = if port = Some 23 then None else port;
				   path = None;
				   data = ""
				 }
			 end
(*
		 | "wais" ->
		 | "file" ->
		      slashslash lexbuf;
		 | "prospero" ->
		 | "xmpp" ->
	*)
		 | _ ->
		      raise UrlError
	      end
	 }
	      
and slashslash = parse
   | "//"
	 { () }
   | _
	 { raise (UrllexingError ("// expected", Lexing.lexeme_start lexbuf)) }

and userpass = parse
   | [^ ':' '/' '@']+ ':' [^ ':' '/' '@']* '@'
	 { let lexeme = Lexing.lexeme lexbuf in
           let pos = String.index lexeme ':' in
              Some (String.sub lexeme 0 pos),
              Some 
		 (String.sub lexeme (succ pos) (String.length lexeme - 2 - pos))
	 }
   | [^ ':' '/' '@']* '@'
	 { let lexeme = Lexing.lexeme lexbuf in
              Some (String.sub lexeme 0 (String.length lexeme - 1)),
              None
	 }
   | ""	 
	 { None, None}

and host = parse
   | ['a'-'z' 'A'-'Z' '0'-'9' '-' '.']+
	 {  Lexing.lexeme lexbuf }
   | _
	 { raise (UrllexingError ("host expected", Lexing.lexeme_start lexbuf))}

and port = parse
   | ':' ['0'-'9']+
	 { let lexeme = Lexing.lexeme lexbuf in
	   let port = String.sub lexeme 1 (String.length lexeme - 1) in
	      Some (int_of_string port)
	 }
   | ""
	{ None }

and url_path = parse
   | '/' [^ '\n']*
	 { Some (Lexing.lexeme lexbuf) }
   | ""
	 { None }

and any = parse
   | [^ '\n']*  { Some (Lexing.lexeme lexbuf) }

{
   let parse str =
      let lexbuf = Lexing.from_string str in
	 url_parse lexbuf

   let to_string url =
      Printf.sprintf "%s%s%s%s%s"
	 (match url.protocol with
	     | HTTP | HTTPS | FTP ->
		  (string_of_protocol url.protocol) ^ "://"
	     | FILE ->
		  string_of_protocol url.protocol ^ ":/"
	     | _ -> string_of_protocol url.protocol ^ ":")
	 (match url.password with
	     | None ->
		  (match url.user with
		      | None -> ""
		      | Some u -> u ^ "@")
	     | Some p ->
		  (match url.user with
		      | None -> ":" ^ p ^ "@"
		      | Some u -> u ^ ":" ^ p ^ "@"))
	 url.host
	 (match url.port with None -> "" | Some p -> ":" ^ string_of_int p)
	 (match url.path with
	     | None ->
		  begin match url.protocol with
		     | HTTP | HTTPS -> "/"
		     | _ -> ""
		  end
	     | Some p -> p)
}
