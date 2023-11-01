type 'a tree = 
    | Leaf
    | Node of 'a tree * 'a * 'a tree

type 'a context =
    | Root
    | Left of 'a context * 'a * 'a tree
    | Right of 'a tree * 'a * 'a context

type 'a zipper = 'a context * 'a tree

let zipper_of_tree t =
    (Root, t)

let rec plug (ctx, t) =
    match ctx with
    | Root -> t
    | Left(ctx, x, r) -> plug (ctx, Node(t, x, r))
    | Right(l, x, ctx) -> plug (ctx, Node(l, x, t))

exception At_root
exception At_leaf

let go_up (ctx, t) =
    match ctx with
    | Root -> raise At_root
    | Left(ctx, x, r) -> (ctx, Node(t, x, r))
    | Right(l, x, ctx) -> (ctx, Node(l, x, t))

let go_up_opt (ctx, t) =
    match ctx with
    | Root -> None
    | Left(ctx, x, r) -> Some (ctx, Node(t, x, r))
    | Right(l, x, ctx) -> Some (ctx, Node(l, x, t))

let go_right (ctx, t) =
    match t with
    | Leaf -> raise At_leaf
    | Node(l, x, r) -> (Right(l, x, ctx), r)

let go_right_opt (ctx, t) =
    match t with
    | Leaf -> None
    | Node(l, x, r) -> Some (Right(l, x, ctx), r)

let go_left (ctx, t) =
    match t with
    | Leaf -> failwith "already at leaf, cannot go left"
    | Node(l, x, r) -> (Left(ctx, x, r), l)

let go_left_opt (ctx, t) =
    match t with
    | Leaf -> None
    | Node(l, x, r) -> Some (Left(ctx, x, r), l)

let get (ctx, t) = t

let set (ctx, _) t = (ctx, t)

let elem (_, t) =
    match t with
    | Leaf -> raise At_leaf
    | Node(_, x, _) -> x

let elem_opt (_, t) =
    match t with
    | Leaf -> None
    | Node(_, x, _) -> Some x

let set_elem (ctx, t) x =
    match t with
    | Leaf -> raise At_leaf
    | Node(l, _, r) -> (ctx, Node(l, x, r))

let set_elem_opt (ctx, t) x =
    match t with
    | Leaf -> None
    | Node(l, _, r) -> Some (ctx, Node(l, x, r))
