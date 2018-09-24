type token = Left_paren| Right_paren| Minus| Plus| Div| Mul |Pow
              | Identifier of string| Number of int

type expr =
  | NumExp of int
  | Unary of token * expr
  | Binary of expr * token * expr
  | Grouping of expr

let rec primary tokens = match tokens with
  | [] -> failwith "Tokens are empty"
  | (Number x)::xs -> (NumExp x), xs
  | Left_paren::xs ->
      let e, tokens = expression xs in
      if tokens = [] || (List.hd tokens) <> Right_paren then failwith "')' missing!"
      else (Grouping e), (List.tl tokens)
  | _ -> failwith "Unexpected token"
and power tokens =
  let rec aux e t = match t with
    | [] -> e, t
    | x::xs -> if x = Pow then
                let m,t = primary xs in
                aux (Binary (e,x,m)) t
              else e,t
  in
  let m, tokens = primary tokens in
  aux m tokens
and unary tokens = match tokens with
  | [] -> power tokens
  | x::xs -> if x = Minus then
             let e, tokens = unary xs in
             (Unary (x, e)), tokens
             else power tokens
and multiplication tokens =
 let rec aux e t = match t with
   | [] -> e, t
   | x::xs -> if x = Mul || x = Div then
                 let m, t = unary xs in
                 aux (Binary (e, x, m)) t
              else e, t
 in
 let m, tokens = unary tokens in
 aux m tokens
and addition tokens =
 let rec aux e t = match t with
   | [] -> e, t
   | x::xs -> if x = Minus || x = Plus then
                 let m, t = multiplication xs in
                 aux (Binary (e, x, m)) t
              else e, t
 in
 let m, tokens = multiplication tokens in
 aux m tokens
and expression tokens = addition tokens
and parse tokens = expression tokens

let pow x y =
  let rec aux y a =
    if a = 1 then y
    else aux (y*x) (a-1)
  in aux x y

let rec eval e = match e with
  | NumExp x -> x
  | Unary (_,x) -> (-(eval x))
  | Binary (x,t,y) ->
      let op = match t with Minus -> (-) | Plus -> (+) | Div -> (/) | Mul -> ( * ) | Pow -> pow in
      op (eval x) (eval y)
  | Grouping x -> eval x

let prog tokens = let ast, _ = parse tokens in eval ast
