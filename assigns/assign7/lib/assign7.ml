open Utils

module Sexpr = Sexpr

module Expr = struct
  type t =
    | Fun of string * t
    | App of t * t
    | Var of string

  let to_string e =
    let rec go = function
      | Fun (x, e) -> Format.sprintf "(lambda %s %s)" x (go e)
      | App (e1, e2) -> Format.sprintf "(%s %s)" (go e1) (go e2)
      | Var x -> x
    in go e

  let ( let* ) = Option.bind

  let is_var s = s <> "" && String.for_all is_lower s

  let rec of_sexpr_opt se =
    match se with
    | Sexpr.Atom x -> if is_var x then Some (Var x) else None
    | Sexpr.List [Sexpr.Atom "lambda"; Sexpr.Atom x; body] when is_var x ->
       let* body = of_sexpr_opt body in
       Some (Fun (x, body))
    | Sexpr.List [se1; se2] ->
       let* e1 = of_sexpr_opt se1 in
       let* e2 = of_sexpr_opt se2 in
       Some (App (e1, e2))
    | _ -> None

  let of_string_opt s =
    let* se = Sexpr.of_string_opt s in
    of_sexpr_opt se

  let of_string s =
    match of_string_opt s with
    | Some e -> e
    | None -> raise (Failure "Expr.of_string")

  let rec fv : t -> VarSet.t = function
    | Fun (x, e) -> VarSet.remove x (fv e)
    | App (e1, e2) -> VarSet.union (fv e1) (fv e2)
    | Var x -> VarSet.singleton x

  let rec subst e' x e =
  match e with
  | Var y -> if x = y then e' else e
  | App (e1, e2) -> App (subst e' x e1, subst e' x e2)
  | Fun (y, body) ->
     if x = y then e                                   
     else if not (VarSet.mem y (fv e')) then Fun (y, subst e' x body)
     else                                             
       let z = gensym () in
       Fun (z, subst e' x (subst (Var z) y body))
  
  let rec equal e1 e2 =
  match e1, e2 with
  | Var x, Var y -> x = y
  | App (a1, b1), App (a2, b2) -> equal a1 a2 && equal b1 b2
  | Fun (x, b1), Fun (y, b2) ->
     let z = Var (gensym ()) in
     equal (subst z x b1) (subst z y b2)
  | _ -> false

end

let rec step e =
  let open Expr in
  match e with
  | App (Fun (x, body), e2) ->                
     Some (subst e2 x body)
  | App (e1, e2) ->
     (match step e1 with
      | Some e1' -> Some (App (e1', e2))      
      | None ->
         match step e2 with                   
         | Some e2' -> Some (App (e1, e2'))
         | None -> None)
  | Fun (x, body) ->                          
     Option.map (fun body' -> Fun (x, body')) (step body)
  | Var _ -> None

let multistep =
  let rec go e =
    match step e with
    | Some e -> go e
    | None -> e
  in go

let interp s =
  match Expr.of_string_opt s with
  | None -> print_endline "parse error"
  | Some e ->
     match multistep e with
     | e -> e |> Expr.to_string |> print_endline

let expr_of_int n =
  let rec body n =
    if n <= 0 then Expr.Var "x" else Expr.App (Expr.Var "f", body (n - 1)) in
  Expr.Fun ("f", Expr.Fun ("x", body n))

let suc = Expr.of_string "(lambda n (lambda f (lambda x (f ((n f) x)))))"

let int_of_expr_opt e =
  let open Expr in
  match e with
  | Fun (f, Fun (x, body)) ->
     let rec count = function
       | Var y when y = x -> Some 0
       | App (Var g, rest) when g = f && f <> x ->
          Option.map (( + ) 1) (count rest)
       | _ -> None
     in count body
  | _ -> None

let int_of_expr e =
  match int_of_expr_opt e with
  | Some n -> n
  | None -> raise (Failure "int_of_expr")

let add = Expr.of_string
  "(lambda m (lambda n (lambda f (lambda x ((m f) ((n f) x))))))"
