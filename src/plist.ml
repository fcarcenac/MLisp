type 'a t = { mutable head: 'a ; tail : 'a t ; mutable pc : 'a t }

let nil v =
  let rec null = { head = v ; tail = null ; pc = null } in
  null

let is_nil l = l.tail == l

let cons x l =
  if is_nil l.pc then 
    begin
      let n = { l with head = x ; tail = l } in
      if not (is_nil l) then l.pc <- n;
      n
    end
  else
    begin
      let c = l.pc in
      c.head <- x;
      c
    end

let head l = l.head
let tail l = l.tail

let rec mem x l = not (is_nil l) && (x = l.head || (mem x l.tail)) 
