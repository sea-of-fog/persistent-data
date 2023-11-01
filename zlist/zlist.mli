type 'a zlist

val of_list : 'a list -> 'a zlist
val to_list : 'a zlist -> 'a list

exception At_front
exception At_end

(* return element after the cursor or raise At_end *)
val elem        : 'a zlist -> 'a
val elem_opt    : 'a zlist -> 'a option

(* move the cursor to the left or raise At_end *)
val move_left       : 'a zlist -> 'a zlist
val move_left_opt   : 'a zlist -> 'a zlist option

(* move the cursor to the right or raise At_front *)
val move_right      : 'a zlist -> 'a zlist
val move_right_opt  : 'a zlist -> 'a zlist option

val insert      : 'a -> 'a zlist -> 'a zlist
(* remove the element before the cursor or raise At_front *)
val remove      : 'a zlist -> 'a zlist
val remove_opt  : 'a zlist -> 'a zlist option
