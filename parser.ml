open Util

type token = Left_paren| Right_paren| Minus| Plus| Div| Mul |Pow| Comma
              | Identifier of string| Number of int | Print | EOL |Let
              | Equal

type tuple = BaseTuple of expr | NextTuple of expr * tuple
and
expr =
  | NumExp of int
  | VarExp of string
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
  | (Identifier x)::xs -> (VarExp x), xs
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
                      | x -> aux (Tuple (NextTuple (e, BaseTuple x))) t
                  )
                else e,t
  in
  let m,tokens = addition tokens in
  aux m tokens
and expression tokens = tuple tokens
and statement tokens = match tokens with
  | [] -> []
  | x::xs -> (
    let current_line,next_line = split_eol xs in
    match x with
      | Print ->
        let e,_ = expression current_line in
        (PrintStmt e)::(statement next_line)
      | Let ->
        let s = (match List.hd current_line with
                | Identifier x -> x | _ -> failwith "Identifier expected.") in
        let current_line = List.tl current_line in
        if not ((List.hd current_line) = Equal) then failwith "Equal is missing." else
        let current_line = List.tl current_line in
        let e,_ = expression current_line in
        (AssignStmt (s, e))::(statement next_line)
    )
and parse tokens = statement tokens
and split_eol l = match l with
  | [] -> [],[]
  | x::xs ->
    if x = EOL then [],xs else
    let a,b = split_eol xs in
    x::a,b

let pow x y =
  let rec aux y a =
    if a = 1 then y
    else aux (y*x) (a-1)
  in aux x y

let unpackNumValue x = match x with
  | NumValue y -> y
  | _ -> failwith "wrong unpack!"

let rec eval_expr e env = match e with
  | NumExp x -> NumValue x
  | VarExp x -> List.assoc x env
  | Unary (_,x) -> NumValue (- unpackNumValue (eval_expr x env))
  | Binary (x,t,y) ->
      let op = match t with Minus -> (-) | Plus -> (+) | Div -> (/) | Mul -> ( * ) | Pow -> pow in
      NumValue (op (unpackNumValue (eval_expr x env)) (unpackNumValue (eval_expr y env)))
  | Tuple l -> TupleValue (
    let rec aux l =
      match l with
      | BaseTuple t -> [eval_expr t env]
      | NextTuple (t, ts) -> (eval_expr t env)::(aux ts)
    in aux l
    )
  | Grouping x -> eval_expr x env

let rec print_value v = match v with
  | NumValue x -> print_int x
  | StringValue x -> print_string x
  | TupleValue l ->
    let rec aux l = (match l with
      | [] -> ()
      | [x] -> print_value x  (*for the last entry in the list*)
      | x::xs -> print_value x; print_string ","; aux xs)
    in print_string "("; aux l; print_string ")"

let rec print_value_newline v = print_value v; print_newline ()

let print_expr e env = print_value_newline (eval_expr e env)

let rec eval p env = match p with
  | [] -> ()
  | x::xs -> let env = (
    match x with
      | PrintStmt e -> (print_expr e env); env
      | AssignStmt (x,e) -> update_assoc (x, eval_expr e env) env
      | _ -> failwith "Not implemented"
    ) in eval xs env

let test_program =
[Let; Identifier "x"; Equal; Number 5; Plus; Number 2; Comma; Number 5; Pow; Number 3; EOL;
Let; Identifier "y"; Equal; Number 7; EOL;
Print; Identifier "y"; Plus; Identifier "y"; EOL;
Print; Identifier "x"; EOL;
Let; Identifier "x"; Equal; Number 163; EOL;
Print; Identifier "x"; Pow; Number 2; Minus; Identifier "y"]

let main = eval (parse test_program) []

let () = main
