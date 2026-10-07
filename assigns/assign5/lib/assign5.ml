
let group (l : int list) : int list list option =
  let same_sign x y = (x > 0) = (y > 0) in
  let rec go l cur acc =
    match l, cur with
    | [], [] -> if acc = [] then Some [] else None
    | [], _ -> Some (List.rev (List.rev cur :: acc))
    | 0 :: _, [] -> None
    | 0 :: rest, _ -> go rest [] (List.rev cur :: acc)
    | x :: rest, [] ->
      (match acc with
       | (y :: _) :: _ -> if same_sign x y then None else go rest [x] acc
       | _ -> go rest [x] acc)
    | x :: rest, y :: _ ->
      if same_sign x y then go rest (x :: cur) acc else None
  in
  go l [] []

type 'a rtree = Node of 'a * 'a rtree list

let split (t : ('a * 'b) rtree) : 'a rtree * 'b rtree =
  let rec go t =
    match t with
    | Node ((a, b), children) ->
      let pairs = List.map go children in
      ( Node (a, List.map (fun (l, _) -> l) pairs)
      , Node (b, List.map (fun (_, r) -> r) pairs) )
  in
  go t

let prefix_map (f : 'a list -> 'b option) (l : 'a list) : ('b * 'a list) option =
  let rec go pre rest =
    match f (List.rev pre) with
    | Some b -> Some (b, rest)
    | None ->
      (match rest with
       | [] -> None
       | x :: xs -> go (x :: pre) xs)
  in
  go [] l

let apply_cycle (f : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
  let rec go fs n x =
    if n = 0 then x
    else
      match fs with
      | [] -> (match f with [] -> x | _ -> go f n x)
      | g :: rest -> go rest (n - 1) (g x)
  in
  go f n x


let walks (f : 'a -> 'a -> bool) (n : int) (ps : (('a -> 'a) * 'a) list) : 'a list =
  let rec path p x k = if k = 0 then [x] else x :: path p (p x) (k - 1) in
  let rec endpoint p x k = if k = 0 then x else endpoint p (p x) (k - 1) in
  let rec valid l =
    match l with
    | a :: b :: rest -> f a b && valid (b :: rest)
    | _ -> true
  in
  ps
  |> List.map (fun (p, x) -> (path p x n, endpoint p x n))
  |> List.filter (fun (pth, _) -> valid pth)
  |> List.map (fun (_, e) -> e)
