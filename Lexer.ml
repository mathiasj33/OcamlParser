open MyUtil
open MyTypes

let keywords = [("let", Let); ("print", Print)]

let match_char c = match c with
  | '(' -> Left_paren
  | ')' -> Right_paren
  | '-' -> Minus
  | '+' -> Plus
  | '/' -> Div
  | '*' -> Mul
  | '^' -> Pow
  | ',' -> Comma
  | '=' -> Equal
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
      else iter_chars (i+1) ((match_char c)::acc)
    in
    lex_rev xs (iter_chars 0 acc)

let lex prog = List.rev (lex_rev (String.split_on_char ' ' prog) [])
