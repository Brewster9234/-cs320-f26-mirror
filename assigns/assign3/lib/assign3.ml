let rec remove_key (k : 'a) (l : ('a * 'b) list) : ('a * 'b) list =
  match l with
  | [] -> []
  | (k', v) :: t -> if k' = k then remove_key k t else (k', v) :: remove_key k t

let rec nub (l : ('a * 'b) list) : ('a * 'b) list =
  match l with
  | [] -> []
  | (k, v) :: t -> (k, v) :: nub (remove_key k t)


  let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then acc
    else loop (s.[i] :: acc) (i + 1)
  in List.rev (loop [] 0)

let implode (l : char list) : string =
  String.init (List.length l) (List.nth l)

let split_by_ws' (s : string) : string list =
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let rec go chars current acc =
    match chars with
    | [] -> if current = [] then List.rev acc else List.rev (implode (List.rev current) :: acc)
    | c :: rest ->
        if is_ws c then
          if current = [] then go rest [] acc
          else go rest [] (implode (List.rev current) :: acc)
        else go rest (c :: current) acc
  in
  go (explode s) [] []
