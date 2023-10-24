type 'a queue

val empty : 'a queue
val enqueue : 'a -> 'a queue -> 'a queue
val peek : 'a queue -> 'a
val peek_opt : 'a queue -> 'a option
val pop : 'a queue -> 'a queue
(* val pop_opt : 'a queue -> 'a queue option *)

