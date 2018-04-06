(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                             Refs Testing
                             Spring 2017
 *)

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons (2, ref Nil) ;;
let list1b = Cons (2, ref list1a) ;;
let list1 = Cons (1, ref list1b) ;;

let reflist = ref (Cons (2, ref Nil)) ;;
let list2 = Cons (1, ref (Cons (2, reflist))) ;;
let _ = reflist := list2 ;;

(* Some example tests. You'll want more. *)
let _ =
  assert(not(has_cycle list1a)) ;
  assert(has_cycle(!reflist)) ;
  assert(has_cycle list1a = false);
  assert(has_cycle list1b = false);
  assert(has_cycle list2 = true);

  assert(flatten list2; has_cycle list2 = false);
  assert(mlength list1a = 1);
  assert(mlength list1b = 2);
  assert(mlength list1 = 3);
  assert(mlength list1b = 2);
  assert(mlength list2 = 2);


