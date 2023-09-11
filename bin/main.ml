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

let length list = 
  let rec counter list k = match list with
  | [] -> k
  | _::xy -> counter xy (k+1)
  in
  counter list 0 

let rec rev list = match list with
| x::xy -> rev xy @ [x]
| [] -> []

let palindrome list = if rev list == list then true else false

type 'a node =
    | One of 'a 
    | Many of 'a node list

let rec flatten list = match list with
| One a::xy -> [a] @ flatten xy
| Many a::xy -> flatten a @ flatten xy  
| [] -> [] 




let () = 
  let _ = last ["a";"b";"c"]
  in
  ()  
