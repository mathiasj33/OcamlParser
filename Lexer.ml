open MyUtil
open MyTypes

let keywords = [("let", Let); ("print", Print); ("false", Boolean false); ("true", Boolean true); ("and", And); ("or", Or)]

let match_char c = match c with
  | '(' -> Left_paren
  | ')' -> Right_paren
  | '-' -> Minus
  | '+' -> Plus
  | '/' -> Div
  | '*' -> Mul
  | '^' -> Pow
  | ',' -> Comma
  | '\n' -> EOL
  | _ -> failwith "Unrecognized symbol."

let rec lex_rev split acc = match split with
  | [] -> acc
  | x::xs ->
    if List.mem_assoc x keywords then lex_rev xs ((List.assoc x keywords)::acc) else
    let rec match_token i f =
      if i >= String.length x then i else
      let c = x.[i] in
      if f c then match_token (i+1) f else i
    in
    let match_identifier i = match_token i is_alpha in
    let match_number i = match_token i is_digit in
    let rec iter_chars i acc =
      if i >= String.length x then acc else
      let c = x.[i] in
      if is_alpha c then
        let j = match_identifier i in
        let sub = String.sub x i (j-i) in
        iter_chars j ((Identifier sub)::acc)
      else if is_digit c then
        let j = match_number i in
        let sub = String.sub x i (j-i) in
        iter_chars j ((Number (int_of_string sub))::acc)
      else if c = '<' then
        if (i < String.length x - 1) && (x.[i+1] = '=') then iter_chars (i+2) (Leq::acc)
        else iter_chars (i+1) (L::acc)
      else if c = '>' then
        if (i < String.length x - 1) && x.[i+1] = '=' then iter_chars (i+2) (Geq::acc)
        else iter_chars (i+1) (G::acc)
      else if c = '=' then
        if (i < String.length x - 1) && x.[i+1] = '=' then iter_chars (i+2) (EqualEqual::acc)
        else iter_chars (i+1) (Equal::acc)
      else iter_chars (i+1) ((match_char c)::acc)
    in
    lex_rev xs (iter_chars 0 acc)

let join l c =
  let rec aux l a = match l with
    | [] -> a
    | x::xs -> aux xs (a @ [x; (String.make 1 c)])
  in aux l []

let rec flatten l = match l with
  | [] -> []
  | x::xs -> x @ (flatten xs)

let lex prog =
  let lines = String.split_on_char '\n' prog in
  let lines = join lines '\n' in
  let mapped = List.map (fun x -> String.split_on_char ' ' x) lines in
  let flattened = flatten mapped in
  List.rev (lex_rev flattened [])
