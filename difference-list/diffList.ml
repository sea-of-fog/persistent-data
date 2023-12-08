type 'a t =
    'a list -> 'a list

(* Constructing lists *)

let empty =
    (fun zs -> zs)

let cons x xs = 
    (fun zs -> x::(xs zs))

let append xs ys =
    (fun zs -> xs (ys zs))

let ( ++ ) xs ys =
    append xs ys

let singleton x =
    (fun zs -> x::zs)

let snoc xs x =
    xs ++ (singleton x)

(* Converting to and from lists *)

let rec of_list xs =
    match xs with
    | []    -> empty
    | x::xs -> cons x (of_list xs)

let prepend xs ys =
    xs ys

let to_list xs =
    prepend xs []

(* Extracting values *)

exception Empty

let is_empty xs =
    match to_list xs with
    | [] -> true
    | _  -> false

let hd xs =
    match to_list xs with
    | x::_ -> x
    | []   -> raise Empty

let hd_opt xs =
    match to_list xs with
    | x::_ -> Some x
    | []   -> None

let tl xs =
    match to_list xs with
    | _::xs -> of_list xs
    | []    -> raise Empty

let tl_opt xs =
    match to_list xs with
    | _::xs -> Some (of_list xs)
    | []    -> None

let uncons xs =
    match to_list xs with
    | x::xs -> (x, of_list xs)
    | []    -> raise Empty

let uncons_opt xs =
    match to_list xs with
    | x::xs -> Some (x, of_list xs)
    | []    -> None

(* Transforming *)
let map f xs =
    of_list @@ List.map f @@ to_list xs

let filter p xs =
    of_list @@ List.filter p @@ to_list xs

let foldr f xs acc =
    List.fold_right f (to_list xs) acc

let rec unfoldr f acc =
    match f acc with
    | None          -> empty
    | Some (x, acc) -> cons x (unfoldr f acc)

let length (xs : 'a t) =
    foldr (fun _ acc -> acc + 1) xs 0
