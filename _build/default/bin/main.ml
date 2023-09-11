let rec last list = match list with
| [] -> None
| [x] -> Some x
| _::xy -> last xy

let () = 
  let _ = last ["a";"b";"c"]
  in
  ()  
