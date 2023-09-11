let rec last list = match list with
| [] -> None
| [x] -> Some x
| _::xy -> last xy

let rec last_one list = match list with
| [] -> None
| [_] -> last_one []
| [x;y] -> Some(x,y)
|  _::xy -> last_one xy

let rec at list k = match list with 
| x::xy -> if k = 0 then Some x else at xy (k-1)
| [] -> None



let () = 
  let _ = last ["a";"b";"c"]
  in
  ()  
