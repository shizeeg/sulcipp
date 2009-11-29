exception UrllexingError of (string * int)

type protocol =
   | FTP | HTTP | HTTPS | NEWS | NNTP | TELNET | WAIS | FILE | PROCPERO | XMPP

type url = {
   protocol: protocol;
   user: string option;
   password: string option;
   host: string;
   port: int option;
   path: string option;
   data: string
}
      
let string_of_protocol p =
      match p with
	 | FTP  -> "ftp"
	 | HTTP -> "http"
	 | HTTPS -> "https"
	 | NEWS -> "news"
	 | NNTP -> "nntp"
	 | TELNET -> "telnet"
	 | WAIS -> "wais"
	 | FILE -> "file"
	 | PROCPERO -> "procpero"
	 | XMPP -> "xmpp"
      
type http_method = | POST | GET | HEAD

