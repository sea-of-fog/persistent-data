(* Persistent queue *)
(* Implemented as two lists, of which one represents the front *)
(* and the second one the back of the list *)
(* Invariant: if the queue is nonempty, the front is nonempty *)

(* TODO: fix pop_opt *)
(* TODO: uncomment pop_opt in .mli *)

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
    | Queue(a, fs, bs) -> begin match fs with
                        | f::fs -> Queue(f, fs, bs)
                        | [] -> begin match bs with
                                      | b::bs -> Queue(b, List.rev bs, [])
                                      | [] -> Empty
                                end
                          end

(* let pop_opt q = *)
(*     match q with *)
(*     | Empty -> None *)
(*     | Queue(a, fs, bs) -> begin match fs with *)
(*                                 | f::fs -> Some Queue(f, fs, bs) *)
(*                                 | [] -> begin match bs with *)
(*                                               | b::bs -> Some Queue(b, List.rev bs, []) *)
(*                                               | [] -> Some Empty *)
(*                                         end *)
(*                           end *)
