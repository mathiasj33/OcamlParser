type token = Left_paren| Right_paren| Minus| Plus| Div| Mul |Pow| Comma
              | Identifier of string| Number of int | Print

type tuple = BaseTuple of expr | NextTuple of expr * tuple
and
expr =
  | NumExp of int
  | Unary of token * expr
  | Binary of expr * token * expr
  | Tuple of tuple
  | Grouping of expr

type value =
  | NumValue of int
  | StringValue of string
  | TupleValue of value list

type stmt =
  | PrintStmt of expr
  | AssignStmt of string * expr

type prog = stmt list

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
and tuple tokens =
  let rec aux e t = match t with
    | [] -> e, t
    | x::xs -> if x = Comma then
                  let m, t = tuple xs in
                  (
                    match m with
                      | Tuple a -> aux (Tuple (NextTuple (e, a))) t
                      | NumExp a -> aux (Tuple (NextTuple (e, BaseTuple (NumExp a)))) t
                      | Unary (a,b) -> aux (Tuple (NextTuple (e, BaseTuple (Unary (a,b))))) t
                      | Binary (a,b,c) -> aux (Tuple (NextTuple (e,BaseTuple (Binary (a,b,c))))) t
                      | Grouping a -> aux (Tuple (NextTuple (e,BaseTuple (Grouping a)))) t
                  )
                else e,t
  in
  let m,tokens = addition tokens in
  aux m tokens
and expression tokens = tuple tokens
and statement tokens = match tokens with
  | [] -> []
  | x::xs -> (
    match x with
      | Print ->
        let e,_ = expression xs in
        [PrintStmt e]
    )
and parse tokens = statement tokens

let pow x y =
  let rec aux y a =
    if a = 1 then y
    else aux (y*x) (a-1)
  in aux x y

let unpackNumValue x = match x with
  | NumValue y -> y
  | _ -> failwith "wrong unpack!"

let rec eval_expr e = match e with
  | NumExp x -> NumValue x
  | Unary (_,x) -> NumValue (- unpackNumValue (eval_expr x))
  | Binary (x,t,y) ->
      let op = match t with Minus -> (-) | Plus -> (+) | Div -> (/) | Mul -> ( * ) | Pow -> pow in
      NumValue (op (unpackNumValue (eval_expr x)) (unpackNumValue (eval_expr y)))
  | Tuple l -> TupleValue (
    let rec aux l =
      match l with
      | BaseTuple t -> [eval_expr t]
      | NextTuple (t, ts) -> (eval_expr t)::(aux ts)
    in aux l
    )
  | Grouping x -> eval_expr x

let rec print_value v = match v with
  | NumValue x -> print_int x
  | StringValue x -> print_string x
  | TupleValue l ->
    let rec aux l = match l with
      | [] -> ()
      | x::xs -> print_value x; print_string ","; aux xs
    in print_string "("; aux l; print_string ")"

let print_expr e = print_value (eval_expr e)

let rec eval p = match p with
  | [] -> ()
  | x::xs -> (
    match x with
      | PrintStmt e -> print_expr e
      | _ -> failwith "Not implemented"
    ); eval xs

let main = eval (parse [Print; Number 5; Plus; Number 2; Comma; Number 5; Pow; Number 3])

let () = main
