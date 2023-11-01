(* lista z kursorem to para (przed_kurs, za_kurs), przy czym "przed" jest przetrzymywana odwrócona *)
type 'a zlist = 'a list * 'a list

let of_list xs = ([], xs)

let to_list zs = List.rev_append (fst zs) (snd zs)

exception At_front
exception At_end

let elem zs =
    match snd zs with
    | [] -> raise At_end
    | x::_ -> x

let elem_opt zs =
    match snd zs with
    | [] -> None
    | x::_ -> Some x

let move_left zs =
    match fst zs with
    | [] -> raise At_front
    | x::xs -> (xs, x::(snd zs))

let move_left_opt zs =
    match fst zs with
    | [] -> None
    | x::xs -> Some (xs, x::(snd zs))

let move_right zs =
    match snd zs with
    | [] -> raise At_end
    | x::xs -> (x::(fst zs), xs)

let move_right_opt zs =
    match snd zs with
    | [] -> None
    | x::xs -> Some (x::(fst zs), xs)

let insert x zs =
    let front, back = zs in
        (x::front, back)

let remove zs =
    match fst zs with
    | [] -> raise At_front 
    | x::xs -> (xs, snd zs)

let remove_opt zs =
    match fst zs with
    | [] -> None
    | x::xs -> Some (xs, snd zs)
