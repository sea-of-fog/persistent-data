(* lista z kursorem to para (przed_kurs, za_kurs), przy czym "przed" jest przetrzymywana odwrócona *)
type 'a zlist = 'a list * 'a list

let of_list xs = ([], xs)

let to_list zs = List.rev_append (fst zs) (snd zs)

let elem zs =
    match snd zs with
    | [] -> None
    | x::_ -> Some x

let move_left zs =
    match fst zs with
    | [] -> ([], snd zs)
    | x::xs -> (xs, x::(snd zs))

let move_right zs =
    match snd zs with
    | [] -> (fst zs, [])
    | x::xs -> (x::(fst zs), xs)

let insert x zs =
    let front, back = zs in
        (x::front, back)

let remove zs =
    match fst zs with
    | [] -> zs
    | x::xs -> (xs, snd zs)
