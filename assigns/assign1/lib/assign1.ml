let rec num_digits n = 
  if n < 0 then num_digits (0 - n)
  else if n < 10 then 1
  else 1 + num_digits (n / 10)

let is_perfect_pow i n =
  let limit = abs n in
  let rec capped_pow_acc k p acc =
    if p = 0 then acc
    else if k = 0 then 0
    else if k = 1 then acc
    else if acc > limit / k then limit + 1
    else capped_pow_acc k (p - 1) (k * acc)
  in
  let capped_pow k p = capped_pow_acc k p 1 in
  let rec binary_search lo hi =
    if lo > hi then false
    else
      let mid = lo + (hi - lo) / 2 in
      let value = capped_pow mid i in
      if value = limit then true
      else if value < limit then binary_search (mid + 1) hi
      else binary_search lo (mid - 1)
  in
  if i = 1 then true
  else if i = 0 then n = 1
  else if i < 0 then (n = 1 || (n = (-1) && (abs i) mod 2 = 1))
  else if not (binary_search 0 limit) then false
  else if n >= 0 then true
  else (abs i) mod 2 = 1

let rec factors_from n d =
  if n = 1 then 0
  else if d * d > n then 1
  else if n mod d = 0 then 1 + factors_from (n / d) d
  else factors_from n (d + 1)

let num_factors n = factors_from n 2

let rec has_leg_b n a b =
  if b>= n then false
  else if (a * a) + (b * b) = n * n then true
  else has_leg_b n a (b + 1)

let rec has_leg_a n a =
  if a >= n then false 
  else if has_leg_b n a a then true
  else has_leg_a n (a + 1)

let is_hypotenuse n = has_leg_a n 1

let rec drop_leading (k: int) (l: int list) : int list =
  match l with
  | h::t when h=k ->
     drop_leading k t
  | _ -> l