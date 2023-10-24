(* Persistent queue *)
(* Implemented as two lists, of which one represents the front *)
(* and the second one the back of the list *)
(* Invariant: if the queue is nonempty, the front is nonempty *)

type 'a queue =
    | Empty
    | Queue of 'a * 'a list * 'a list

let empty = Empty

let enqueue a q =
    match q with
    | Empty -> Queue(a, [], [])
    | Queue(f, fs, bs) -> Queue(f, fs, a::bs)

let peek q =
    match q with
    | Empty -> failwith "Empty queue"
    | Queue(a, _, _) -> a

let peek_opt q =
    match q with
    | Empty -> None
    | Queue(a, _, _) -> Some a

let pop q =
    match q with
    | Empty -> failwith "Empty queue"
    | Queue(a, f::fs, bs) -> Queue(f, fs, bs)
    | Queue(a, [], bs) -> let fs = List.rev bs
                              in begin match fs with
                                       | f::fs -> Queue(f, fs, [])
                                       | [] -> Empty end

let pop_opt q =
    match q with
    | Empty -> None
    | Queue(a, f::fs, bs) -> Some (Queue(f, fs, bs))
    | Queue(a, [], bs) -> let fs = List.rev bs
                              in begin match fs with
                                       | f::fs -> Some (Queue(f, fs, []))
                                       | [] -> Some (Empty) end
