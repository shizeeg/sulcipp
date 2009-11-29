let _ =
   let file = Sys.argv.(1) in
   let cfg = Ini_config.parse file in
      List.iter (fun (s, kvl) ->
		    Printf.printf "[%s]\n" s;
		    List.iter (fun (k,v) ->
				  Printf.printf "[%s] = [%s]\n" k v
			      ) kvl
		) cfg;

      let section = Sys.argv.(2) in
      let key = Sys.argv.(3) in
      let t = Sys.argv.(4) in
	 match t with
	    | "list"  ->
		 let list = Ini_config.get_value_list cfg section key in
		    List.iter (fun l -> print_endline l) list
	    | "bool" ->
		 let v = Ini_config.get_value_boolean cfg section key in
		    Printf.printf "%s = %s\n" key (string_of_bool v)
	    | _ ->
		 let v = Ini_config.get_value cfg section key in
		    Printf.printf "%s = [%s]\n" key v
