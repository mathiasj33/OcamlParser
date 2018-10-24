open MyUtil
open MyTypes
open Lexer

let rec primary tokens = match tokens with
  | [] -> failwith "Tokens are empty"
  | (Number x)::xs -> (NumExp x), xs
  | (Identifier x)::xs -> (VarExp x), xs
  | (Boolean x)::xs -> (BoolExp x), xs
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
and comparison tokens =
  let rec aux e t = match t with
    | [] -> e, t
    | x::xs -> if x = L || x = Leq || x = G || x = Geq then
                  let m, t = tuple xs in
                  aux (Comp (e, x, m)) t
               else e,t
   in
   let m, tokens = tuple tokens in
   aux m tokens
 and boolOp tokens =
   let rec aux e t = match t with
     | [] -> e, t
     | x::xs -> if x = And || x = Or then
                   let m, t = comparison xs in
                   aux (BoolBinary (e, x, m)) t
                else e,t
    in
    let m, tokens = comparison tokens in
    aux m tokens
and equality tokens =
  let rec aux e t = match t with
    | [] -> e, t
    | x::xs -> if x = EqualEqual then
                  let m, t = boolOp xs in
                  aux (EqualBinary (e, m)) t
               else e,t
   in
   let m, tokens = boolOp tokens in
   aux m tokens
and expression tokens = equality tokens
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

let unpackBoolValue x = match x with
  | BoolValue y -> y
  | _ -> failwith "wrong unpack!"

let rec eval_expr e env = match e with
  | NumExp x -> NumValue x
  | VarExp x -> List.assoc x env
  | BoolExp x -> BoolValue x
  | Unary (_,x) -> NumValue (- unpackNumValue (eval_expr x env))
  | Binary (x,t,y) ->
      let op = match t with Minus -> (-) | Plus -> (+) | Div -> (/) | Mul -> ( * ) | Pow -> pow in
      NumValue (op (unpackNumValue (eval_expr x env)) (unpackNumValue (eval_expr y env)))
  | Comp (x,t,y) ->
    let op = match t with L -> (<) | Leq -> (<=) | G -> (>) | Geq -> (>=) in
    BoolValue (op (unpackNumValue (eval_expr x env)) (unpackNumValue (eval_expr y env)))
  | BoolBinary (x,t,y) ->
    let op = match t with And -> (&&) | Or -> (||) in
    BoolValue (op (unpackBoolValue (eval_expr x env)) (unpackBoolValue (eval_expr y env)))
  | EqualBinary (x,y) ->
    BoolValue ((eval_expr x env) = (eval_expr y env))
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
  | BoolValue x -> (match x with true -> print_string "true" | false -> print_string "false")
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
"let x = 5
 let y = false
 let z = y or true
 print z
 let xx = x > 3
 print xx
 let xy = 10 > 11 -5
 print xy == xx
 print 5 < 3 == 2*3 > 5"

(*let z = y or true
print z
let xx = x > 3
print xx
let xy = 10 > 11 -5
print xy == xx*)

let main = eval (parse (lex test_program)) []

let () = main
