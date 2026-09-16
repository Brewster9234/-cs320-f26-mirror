
let taxicab n =
  let rec count_b a b =
    let s = (a * a * a) + (b * b * b) in
    if s > n then 0
    else if s = n then 1 + count_b a (b + 1)
    else count_b a (b + 1)
  in
  let rec count_a a =
    if (a * a * a) + (a * a * a) > n then 0
    else count_b a a + count_a (a + 1)
  in
  count_a 1
  
let drop_trailing k l =
  let rec drop_leading k l =
    match l with h :: t when h = k -> drop_leading k t | _ -> l
  in
  List.rev (drop_leading k (List.rev l))

let every_k k l =
  let rec go count l =
    match l with
    | [] -> []
    | h :: t -> if count = 0 then h :: go (k - 1) t else go (count - 1) t
  in
  go 0 l

let is_bitonic l =
  let rec go direction switched l =
    match l with
    | a :: b :: rest ->
        if a = b then false
        else
          let d = if a < b then 1 else -1 in
          if direction = 0 then go d switched (b :: rest)
          else if direction = d then go d switched (b :: rest)
          else if switched then false
          else go d true (b :: rest)
    | _ -> true
  in
  go 0 false l

let factor n =
  let rec go n d =
    if n = 1 then []
    else if d * d > n then [ (n, 1) ]
    else if n mod d = 0 then
      let rec count_exp n e =
        if n mod d = 0 then count_exp (n / d) (e + 1) else (n, e)
      in
      let n', e = count_exp n 0 in
      (d, e) :: go n' (d + 1)
    else go n (d + 1)
  in
  go n 2

type path = int * (bool * int) list

let is_valid (p1 : path) (p2 : path) : bool =
  let positions (start, steps) =
    let rec go pos steps =
      match steps with
      | [] -> [ pos ]
      | (dir, dist) :: rest ->
          let next = if dir then pos + dist else pos - dist in
          pos :: go next rest
    in
    go start steps
  in
  let pos1 = positions p1 in
  let pos2 = positions p2 in
  let rec check l1 l2 =
    match (l1, l2) with
    | a1 :: a2 :: rest1, b1 :: b2 :: rest2 ->
        if (a1 - b1) * (a2 - b2) <= 0 then false
        else check (a2 :: rest1) (b2 :: rest2)
    | _ -> true
  in
  check pos1 pos2