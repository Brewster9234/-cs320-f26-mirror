
let is_ws c = c = ' ' || c = '\n' || c = '\r' || c = '\t' || c = '\012'

let split_by_ws (s : string) : string list =
  let n = String.length s in
  let add_word start i acc =
    if start < i then String.sub s start (i - start) :: acc else acc
  in
  let rec go i start acc =
    if i >= n then List.rev (add_word start i acc)
    else if is_ws s.[i] then go (i + 1) (i + 1) (add_word start i acc)
    else go (i + 1) start acc
  in
  go 0 0 []

type dir = N | S | E | W

let dist (dirs : dir list) : float =
  let rec go x y dirs =
    match dirs with
    | [] -> sqrt (float_of_int (x * x + y * y))
    | N :: rest -> go x (y + 1) rest
    | S :: rest -> go x (y - 1) rest
    | E :: rest -> go (x + 1) y rest
    | W :: rest -> go (x - 1) y rest
  in
  go 0 0 dirs

type int_or_string
  = Int of int
  | String of string

type int_list_or_string_list
  = Int_list of int list
  | String_list of string list

let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec go l =
    match l with
    | [] -> []
    | Int n :: rest ->
      (match go rest with
       | Int_list ns :: groups -> Int_list (n :: ns) :: groups
       | groups -> Int_list [n] :: groups)
    | String s :: rest ->
      (match go rest with
       | String_list ss :: groups -> String_list (s :: ss) :: groups
       | groups -> String_list [s] :: groups)
  in
  go l

type 'a tree
  = Empty
  | Node of 'a * 'a tree * 'a tree

let rec insert (x : 'a) (t : 'a tree) : 'a tree =
  match t with
  | Empty -> Node (x, Empty, Empty)
  | Node (a, left, right) ->
    if x <= a
    then Node (a, insert x left, right)
    else Node (a, left, insert x right)

let rec flatten (t : 'a tree) : 'a list =
  match t with
  | Empty -> []
  | Node (a, left, right) -> flatten left @ (a :: flatten right)

let rec sort (l : 'a list) : 'a list =
  let rec build l t =
    match l with
    | [] -> t
    | x :: rest -> build rest (insert x t)
  in
  flatten (build l Empty)
