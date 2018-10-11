let update_assoc (k,v) l = (k, v)::(List.remove_assoc k l)

let is_digit c =
  let code = Char.code c in
  code >= 48 && code <= 57

let is_alpha c =
  let code = Char.code c in
  code >= 97 && code <= 122
