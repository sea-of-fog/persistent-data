type 'a tree = 
    | Leaf
    | Node of 'a tree * 'a * 'a tree

type 'a zipper

(* Setting the cursor at the root of the tree *)
val zipper_of_tree : 'a tree -> 'a zipper

(* Forgetting the cursor *)
val plug : 'a zipper -> 'a tree

exception At_root
exception At_leaf

(* Moving the cursor *)

(* raises At_root if the the cursor is at the root *)
val go_up     : 'a zipper -> 'a zipper
val go_up_opt : 'a zipper -> 'a zipper option

(* these raise At_leaf if the cursor is at the leaf *)
val go_right    : 'a zipper -> 'a zipper
val go_right_opt: 'a zipper -> 'a zipper option
val go_left     : 'a zipper -> 'a zipper
val go_left_opt : 'a zipper -> 'a zipper option

(* extracting and setting the whole tree at the cursor *)
val get : 'a zipper -> 'a tree
val set : 'a zipper -> 'a tree -> 'a zipper 

(* extracting and setting the element at the cursor *)
(* elem and set_elem raise At_leaf if the cursor is at the leaf *)
val elem        : 'a zipper -> 'a
val elem_opt    : 'a zipper -> 'a option
val set_elem    : 'a zipper -> 'a -> 'a zipper
val set_elem_opt: 'a zipper -> 'a -> 'a zipper option
