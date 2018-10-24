type token = Left_paren| Right_paren| Minus| Plus| Div| Mul |Pow| Comma
              | Identifier of string| Number of int | Print | EOL |Let
              | Equal | EqualEqual | L | G | Leq | Geq | Boolean of bool | Or | And

type tuple = BaseTuple of expr | NextTuple of expr * tuple
and
expr =
  | NumExp of int
  | VarExp of string
  | BoolExp of bool
  | Unary of token * expr
  | Binary of expr * token * expr
  | Comp of expr * token * expr
  | BoolBinary of expr * token * expr
  | EqualBinary of expr * expr
  | Tuple of tuple
  | Grouping of expr

type value =
  | NumValue of int
  | StringValue of string
  | BoolValue of bool
  | TupleValue of value list

type stmt =
  | PrintStmt of expr
  | AssignStmt of string * expr

type prog = stmt list
