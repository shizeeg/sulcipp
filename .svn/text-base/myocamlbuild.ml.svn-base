open Ocamlbuild_plugin
open Ocamlbuild_pack.Ocamlbuild_where
open Command
open Myocamlbuild_config

let make_binding t =
  ocaml_lib (t.path ^ "/ml" ^ t.name);
  
  flag ["ocamlmklib"; "c"; ("use_" ^ t.name ^ "_clib")] &
    if t.lib_dir = "" then S[A t.lib] else S[A t.lib_dir; A t.lib];
               
  if t.include_dir <> "" then
    flag ["c"; "compile"; ("include_" ^ t.name ^ "_clib")] &
      S[A"-ccopt"; A t.include_dir];

  flag ["link"; "ocaml"; "library"; ("use_" ^ t.name ^ "_clib")] &
    if t.lib_dir = "" then S[A"-cclib"; A t.lib]
    else S[A"-ccopt"; A t.lib_dir; A"-cclib"; A t.lib];
  
  (* If `static' is true then every ocaml link in bytecode will add
     -custom *)
  if static then
    flag ["link"; "ocaml"; "byte"] (A"-custom");
  
  flag ["link"; "library"; "ocaml"; "byte"; ("use_libml" ^ t.name)] &
    (S[A"-dllpath"; A(t.path); A"-dllib"; A("-lml"^t.name);
       A"-cclib"; A("-L"^t.path); A"-cclib"; A("-lml"^t.name)]);

  flag ["link"; "library"; "ocaml"; "native"; "use_libml" ^ t.name] &
    S[A"-ccopt"; A("-L" ^ t.path); A"-cclib"; A("-lml" ^ t.name)];
  
  (* When ocaml link something that use the libcryptokit,
     then one need that file to be up to date. *)
  dep  ["link"; "ocaml"; ("use_libml" ^ t.name)]
    [t.path ^ "/libml" ^ t.name ^ ".a"];
  
  (* This will import headers in the build directory. *)
  if t.headers <> [] then
    dep  ["compile"; "c"] t.headers
      
let ocamlfind_query pkg =
  let cmd = Printf.sprintf
    "%s/ocamlfind query %s" !bindir (Filename.quote pkg) in
    Ocamlbuild_pack.My_unix.run_and_open cmd
      (fun ic ->
         (* Log.dprintf 5 "Getting Ocaml directory from command %s" cmd; *)
         input_line ic)

let tags =
  ["use_bigarray"; "use_dbm"; "use_dynlink"; "use_graphics"; "" ^
     "use_nums"; "use_str"; "use_toplevel"; "use_unix"; "use_threads"]

let pkg_tags = ref []

let rec ocamlfind_get_deps predicates pkg =
  let rec aux_parse acc ic =
    let line = try Some (input_line ic) with _ -> None in
      match line with
        | None -> S (List.rev acc)
        | Some v ->
            let r = Ocamlbuild_pack.Lexers.space_sep_strings &
              Lexing.from_string v in
            let pkg, tail = List.hd r, List.tl r in
              if List.mem ("use_" ^ pkg) tags then
                aux_parse acc ic
              else if List.mem ("use_" ^ pkg, predicates) !pkg_tags then
                aux_parse acc ic
              else (
                pkg_tags := ("use_" ^ pkg, predicates) :: !pkg_tags;
                let deps = ocamlfind_get_deps predicates pkg in
                let rest = S[deps; A"-I"; A(List.hd tail);
                             S(List.map (fun a -> A a) (List.tl tail))]
                in
                  aux_parse (rest :: acc) ic
              )
  in
  let cmd = Printf.sprintf
    "%s/ocamlfind query -r -predicates %s -format \"%%p %%d %%A\" %s"
    !bindir predicates pkg in
    Ocamlbuild_pack.My_unix.run_and_open cmd &
      aux_parse []


let add_package pkg =
  let use_pkg = "use_" ^ pkg in
    flag ["ocaml"; "compile"; use_pkg] &
      S[A"-I"; A(ocamlfind_query pkg)];
    flag ["ocaml"; "link"; "native"; use_pkg] &
      ocamlfind_get_deps "native,mt" pkg;
    flag ["ocaml"; "link"; "byte"; use_pkg] &
      ocamlfind_get_deps "byte,mt" pkg
      
let cryptokit_dir =
  let cmd = Printf.sprintf "%s/ocamlfind query cryptokit" !bindir in
    try
      Ocamlbuild_pack.My_unix.run_and_open cmd
        (fun ic -> input_line ic)
    with _ ->
      !libdir

let sulci_plugins () =
  let cmd = "cat sulci/plugins.list" in
    Ocamlbuild_pack.Lexers.blank_sep_strings &
      Lexing.from_string &
      Ocamlbuild_pack.My_unix.run_and_read cmd
  

let _ =
  dispatch begin function
    | After_rules ->
        if enable_lua then
          make_binding lua_binding;
        if enable_resolv then
          make_binding resolv_binding;
        if enable_tls then
          make_binding tls_binding;
        if enable_zlib then
          make_binding zlib_binding;

        (* logger *)
        flag ["ocaml"; "pp"; "use_logger.syntax"] &
          S[A"camlp4o"; A"-I"; A"libs/logger"; A"pa_logger.cma"];
        dep ["ocaml"; "ocamldep"; "use_logger.syntax"]
          ["libs/logger/pa_logger.cma"];
        ocaml_lib "libs/logger/logger";

        ocaml_lib ~extern:true ~dir:"+lablgtk2" "lablgtk";

        (* The version number *)
        rule "sulci/version.ml"
          ~prod:"sulci/version.ml"
          ~deps:["sulci/version.ml.src"; "sulci/VERSION"]
          (fun _ _ ->
             let version = with_input_file "sulci/VERSION" input_line in
               Seq [rm_f "sulci/version.ml";
                    Cmd (S[A"sed"; A"-e";
                           A(Format.sprintf "s,VERSION,%s," version);
                           Sh"<"; P"sulci/version.ml.src"; Sh">";
                           Px"sulci/version.ml"]);
                    chmod (A"-w") "sulci/version.ml"]
          );
        
        dep ["native"; "compile"; "ocaml"; "use_plugins"]
          (List.map ((/) "sulci")
             (List.map (fun a -> a ^ ".cmx") (sulci_plugins ())));
        
        dep ["byte"; "compile"; "ocaml"; "use_plugins"]
          (List.map ((/) "sulci")
             (List.map (fun a -> a ^ ".cmo") (sulci_plugins ())));
        
        flag ["ocaml"; "compile"; "include_lang"] & S[A"-I"; A"sulci"];

        dep ["native"; "compile"; "ocaml"; "use_lang"]
          (List.map ((/) "sulci")
             (List.map (fun a -> a ^ ".cmx")
                ["lang/ru_time"; "lang/en_time"; "lang/es_time"]));
        
        dep ["byte"; "compile"; "ocaml"; "use_lang"]
          (List.map ((/) "sulci")
             (List.map (fun a -> a ^ ".cmo")
                ["lang/ru_time"; "lang/en_time"; "lang/es_time"]));
        
        flag ["ocaml"; "pp"; "use_ulex.syntax"] &
          S[A"-I"; A (ocamlfind_query "ulex"); A"pa_ulex.cma"];
        
        flag ["ocaml"; "link"; "native"; "use_cryptokit"] &
          S[A"nums.cmxa"];
        flag ["ocaml"; "link"; "byte"; "use_cryptokit"] &
          S[A"nums.cma"];
        
        ocaml_lib ~extern:true ~dir:cryptokit_dir "cryptokit";
        
        add_package "netsys";
        add_package "ulex";
        add_package "pcre";
        add_package "netclient";
        add_package "equeue";
        add_package "netstring";
        add_package "sqlite3";


    | _ -> ()
  end
