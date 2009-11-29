(*                                                                          *)
(* (c) 2004, 2005, 2006 Anastasia Gornostaeva. <ermine@ermine.pp.ru>        *)
(*                                                                          *)

open Service

let _ =
   let cfg = Config.config Version.name Version.version in
      start ~cfg ()
