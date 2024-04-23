(* Eager binary min heaps, with all invariants encoded in *)
(* their types! *)

(* encoding natural numbers at the type-level *)
type zero =
    Zero
type 'a succ = 
    Succ of 'a

(* the type encodes the rank k of the tree B_k *)
type _ bin_tree =
    | Base : int               -> zero bin_tree
    | Comp : int * (zero, 'a) bin_list -> ('a succ) bin_tree

(* the type encodes: the rank of the first and last elements in the list *)
and (_, _) bin_list =
    | End  : 'a bin_tree -> ('a, 'a) bin_list
    | Cons : 'a bin_tree * ('a succ, 'b) bin_list -> ('a, 'b) bin_list

let min v1 v2 =
    if v1 < v2 then v1 else v2

let max v1 v2 =
    if v1 < v2 then v2 else v1

let rec insert_bin_list : type a b. (b succ) bin_tree -> (a, b) bin_list -> (a, b succ) bin_list = fun t ts ->
    match ts with
    | End(t2)      -> Cons(t2, End(t))
    | Cons(t1, ts) -> Cons(t1, insert_bin_list t ts)

let link : type a. a bin_tree -> a bin_tree -> (a succ) bin_tree = fun t1 t2 ->
    match t1, t2 with
    | Base(v1), Base(v2) -> 
        Comp ((min v1 v2), (End (Base (max v1 v2))))
    | Comp(v1, ts1), Comp(v2, ts2) when v1 < v2 ->
        Comp(v1, insert_bin_list t2 ts1)
    | Comp(v1, ts1), Comp(v2, ts2) ->
        Comp(v2, insert_bin_list t1 ts2)

let root : type a. a bin_tree -> int = fun t ->
    match t with
    | Base(v)    -> v
    | Comp(v, _) -> v

type _ bin_heap =
    | Empty : 'a bin_heap
    | On    : 'a bin_tree * ('a succ) bin_heap -> 'a bin_heap
    | Off   : ('a succ) bin_heap               -> 'a bin_heap

let rec meld : type a. a bin_heap -> a bin_heap -> a bin_heap = fun h1 h2 ->
    match h1, h2 with
    | Empty, _ 
        -> h2
    | _, Empty 
        -> h1
    | Off(h1), Off(h2) ->
        Off(meld h1 h2)
    | On(t1, h1), Off(h2) ->
        On(t1, meld h1 h2)
    | Off(h1), On(t2, h2) ->
        On(t2, (meld h1 h2))
    | On(t1, h1), On(t2, h2) ->
        Off(meld_carry h1 h2 (link t1 t2))

and meld_carry : type a. a bin_heap -> a bin_heap -> a bin_tree -> a bin_heap = fun h1 h2 carry ->
    match h1, h2 with
    | Empty, Empty ->
        On(carry, Empty)
    | Empty, Off(h2) ->
        On(carry, h2)
    | Empty, On(t2, h2) ->
        Off(meld_carry Empty h2 (link carry t2))
    | Off(h1), Empty ->
        On(carry, h1)
    | Off(h1), Off(h2) ->
        On(carry, meld h1 h2)
    | Off(h1), On(t2, h2) ->
        Off(meld_carry h1 h2 (link carry t2))
    | On(t1, h1), Empty ->
        Off(meld_carry h1 Empty (link t1 carry))
    | On(t1, h1), Off(h2) ->
        Off(meld_carry h1 h2 (link t1 carry))
    | On(t1, h1), On(t2, h2) ->
        On(carry, meld_carry h1 h2 (link t1 t2))

let singleton (v : int) : zero bin_heap =
    On(Base v, Empty)

let insert : int -> zero bin_heap -> zero bin_heap = fun v h ->
    meld (singleton v) h

type ext_int =
    | Infty
    | Int of int

let ext_min (v1 : ext_int) (v2 : ext_int) : ext_int =
    match v1, v2 with
    | Infty, v2          -> v2
    | v1, Infty          -> v1
    | (Int v1), (Int v2) -> Int (min v1 v2)

let rec heap_min : type a. a bin_heap -> ext_int = fun h ->
    match h with
    | Empty    -> Infty
    | Off(h)   -> heap_min h
    | On(t, h) -> ext_min (Int (root t)) (heap_min h)
    
let rec heap_of_binlist : type a b. (a, b) bin_list -> a bin_heap = fun ts ->
    match ts with
    | End(t)      -> On(t, Empty)
    | Cons(t, ts) -> On(t, heap_of_binlist ts)

let delete_root : type a. a bin_tree -> zero bin_heap = fun t ->
    match t with
    | Base(_)     -> Empty
    | Comp(_, ts) -> heap_of_binlist ts

let rec delete_first_one : type a. a bin_heap -> a bin_heap = fun h ->
    match h with
    | Empty     -> Empty
    | On(t1, h) -> Off(h)
    | Off(h)    -> Off(delete_first_one h)

let rec delete_help : type a. a bin_heap -> ext_int -> (zero bin_heap * a bin_heap) = fun h mval ->
    match h with
    | Empty  ->
        (Empty, Empty)
    | Off(h) ->
        let (h_tree, h_del) = delete_help h mval in
            (h_tree, Off(h_del))
    | On(t, h) when Int (root t) = mval ->
        (delete_root t, Off(h))
    | On(t, h) -> 
        let (h_tree, h_del) = delete_help h mval in
            (h_tree, On(t, h_del))

let delete_min : zero bin_heap -> zero bin_heap = fun h ->
    let (h_tree, h_del) = delete_help h (heap_min h) in
        meld h_tree h_del
