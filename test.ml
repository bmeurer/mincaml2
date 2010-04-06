
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Pierre Weis, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  only by permission.                                                *)
(*                                                                     *)
(***********************************************************************)

(*                         E I G H T   Q U E E N S

 The Eight Queens Program tail recursive version.

*)

let length l =
  let rec loop accu = function
    | [] -> accu
    | _ :: l -> loop (accu + 1) l in
  loop 0 l;;

let map f l =
 let rec loop accu = function
   | [] -> accu
   | x :: l -> loop (f x :: accu) l in
 loop [] l;;

let rec interval n m =
 if n > m then [] else n :: interval (n + 1) m;;

let rev_append l1 l2 =
  let rec loop accu = function
    | [] -> accu
    | h :: t -> loop (h :: accu) t in
  loop l2 l1;;

let filter_append p l l0 =
  let rec loop accu = function
    | [] -> accu
    | h :: t -> if p h then loop (h :: accu) t else loop accu t in
  let rev_res = loop [] l in
  rev_append rev_res l0;;

let concmap f l =
  let rec loop accu = function
  | [] -> accu
  | h :: t -> loop (f h accu) t in
  loop [] l;;

let rec safe x d  = function
  | [] -> true
  | h :: t ->
     if x <> h then if x <> h + d then if x <> h - d then safe x (d + 1) t else false else false else false;;

let rec ok = function
  | [] -> true
  | h :: t -> safe h 1 t;;

let find_solutions size =
 let line = interval 1 size in
 let rec gen n size =
   if n = 0 then [[]] else
   concmap 
    (fun b -> filter_append ok (map (fun q -> q :: b) line))
    (gen (n - 1) size) in
 gen size size;;

let print_result size =
  let solutions = find_solutions size in
  let sol_num = length solutions in
    print_string "The ";
    print_int size;
    print_string " queens problem has ";
    print_int sol_num;
    print_endline " solutions.";
    print_newline ();
;;

(* 3. Main program. *)

print_result 12;;
