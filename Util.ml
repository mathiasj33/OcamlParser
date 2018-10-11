let update_assoc (k,v) l = (k, v)::(List.remove_assoc k l)
