(*
                          CS51 Problem Set 6
                       Refs, Streams, and Music
                 Section 1: Mutable Lists and Cycles
                             Spring 2018
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)

  let has_cycle (lst : 'a mlist) : bool =
  let rec helper (mList: 'a mlist) (cList : 'a mlist) : bool = 
  	match mList, cList with
  	| Nil, _ -> false
  	| _, Nil -> false
  	| Cons (_, tl), Cons (_, tl2) -> match !tl2 with
  					  | Nil -> false
  					  | Cons (_, tl3) -> if !tl == !tl3 then true else
  					  						helper !tl !tl3 in
  helper lst lst ;;

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)

let flatten (lst : 'a mlist) : unit =
  let rec helper (mList : 'a mlist) (cList : 'a mlist) : unit = 
  	match mList, cList with
  	| Nil , _-> ()
  	| _, Nil -> ()
  	| Cons (_, tl), Cons (_, tl2) -> match !tl2 with
  					  | Nil -> ()
  					  | Cons (_, tl3) -> if !tl == !tl3 then (tl := Nil) else
  					  	helper !tl !tl3 in 
  helper lst lst ;;

(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)

let mlength (lst : 'a mlist) : int =
  let rec length (mList : 'a mlist) (cList : 'a mlist) (acc : int) : int = 
    match mList, cList with
    | Nil, _-> acc 
    | _, Nil -> acc
    | Cons (_, tl), Cons(_, tl2) -> match !tl2 with
              | Nil -> acc + 1
              | Cons (_, tl3) -> if !tl == !tl3 then acc else 
                length !tl !tl3 (acc + 2) in
length lst lst 0 ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 360 ;;
