let rec num_digits n = 
  if n < 0 then num_digits (0 - n)
  else if n < 10 then 1
  else 1 + num_digits (n / 10)

let rec pow k i = if i = 0 then 1 else k * pow k (i - 1)

let rec check k i n =
  if k > abs n then false
  else if pow k i = n then true
  else if pow (0 - k) i = n then true
  else check (k + 1) i n

let is_perfect_pow i n = check 0 i n

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