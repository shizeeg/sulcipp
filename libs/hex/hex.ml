(*                                                                          *)
(* (c) 2004, Anastasia Gornostaeva. <ermine@ermine.pp.ru>                   *)
(*                                                                          *)

let hex_to_digit i =
   match i with
      | x when '0' <= x && x <= '9'  -> 
	   (Char.code x) - (Char.code '0')
      | 'A' | 'a' -> 10
      | 'B' | 'b' -> 11
      | 'C' | 'c' -> 12
      | 'D' | 'd' -> 13
      | 'E' | 'e' -> 14
      | 'F' | 'f' -> 15
      | _ -> failwith "Unexpected letter"

let hex_to_int hex =
   let len = String.length hex in
   let rec cycle acc i =
      if i = len then acc
      else cycle (acc * 16 + (hex_to_digit hex.[i])) (i+1)
   in
      cycle 0 0

let int_to_hex i =
   let rec aux_hex rest acc =
      if rest = 0 then acc
      else
	 let b = rest mod 16 in
	 let h =
	    if b < 10 then Char.chr (b + 48)       (* 48 = Char.code '0' *)
	    else Char.chr (b + 55)                 (* 55 = Char.code 'A' - 10 *)
	 in
	    aux_hex (rest lsr 4) (String.make 1 h ^ acc)
   in
      aux_hex i ""
