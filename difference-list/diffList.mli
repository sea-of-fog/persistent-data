(* 

This is a list-like data structure for constant-time appending,
with linear-time conversion to and from lists.

TODO:

1. [  ] See if I can define pattern matching for this type
2. [  ] See if I can add laziness in a way such that unconsing doesn't cost too much
        (right now hd, tl, uncons need to convert to a list and back)

Cool links: 

1. Wikipedia: https://en.wikipedia.org/wiki/Difference_list 
2. Paper: https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf 
3. Haksell wiki: https://wiki.haskell.org/Difference_list 
4. Haskell library: https://hackage.haskell.org/package/dlist 

*)

type 'a t

(* Construction *)
val empty     : 'a t
val cons      : 'a -> 'a t -> 'a t
val snoc      : 'a t -> 'a -> 'a t
val singleton : 'a -> 'a t
val append    : 'a t -> 'a t -> 'a t
val ( ++ )    : 'a t -> 'a t -> 'a t

(* Converting between types *)
val prepend : 'a t -> 'a List.t -> 'a List.t
val of_list  : 'a List.t -> 'a t
val to_list  : 'a t -> 'a List.t

(* Extracting *)
exception Empty
val is_empty    : 'a t -> bool 
val hd          : 'a t -> 'a
val hd_opt      : 'a t -> 'a option
val tl          : 'a t -> 'a t
val tl_opt      : 'a t -> 'a t option
val uncons      : 'a t -> ('a * 'a t)
val uncons_opt  : 'a t -> ('a * 'a t) option

(* Transforming *)
val map     : ('a -> 'b) -> 'a t -> 'b t
val filter  : ('a -> bool) -> 'a t -> 'a t
val foldr   : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val unfoldr : ('acc -> ('a * 'acc) option) -> 'acc -> 'a t
val length  : 'a t -> int
