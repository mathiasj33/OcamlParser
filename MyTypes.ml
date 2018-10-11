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
