open Command

type cursor = int * int

let update_cursor (line:int) (ind:int) : cursor= 
  (line,ind)

let get_line (c:cursor) : int =
  match c with
  |(line,ind) -> line

let get_ind (c:cursor) : int =
  match c with
  |(line,ind) -> ind

let rec show (txtlist:string list) (c:cursor) (count : int)=
  match txtlist with
  |[] -> ();
  |h :: t -> begin match c with 
      |(line,ind) -> if line = count then
          let end_len = (String.length h) - (ind+1) in
          print_string (String.sub h 0 ind);
          ANSITerminal.(print_string [on_red] (String.sub h ind 1));
          print_string ((String.sub h (ind+1) end_len)^"\n");
        else print_endline h;
        show t c (count+1) end

(** [list_get lst ind counter] returns the [ind]th element of the provided
    list [lst].
    Not_found is raised if lst is empty.*)
let rec list_get lst ind counter : string=
  match lst with
  |[] -> raise Not_found
  |h :: t -> if ind = counter then h
    else list_get t ind (counter+1)

let update_left (c:cursor) (txtlist:string list) : cursor=
  match c with 
  |(line,ind) when (ind > 0)-> update_cursor line (ind-1)
  |(line,ind) when (line > 0) -> let prev = list_get txtlist (line-1) 0 in
    let last_ind = (String.length prev)-1 in 
    update_cursor (line-1) last_ind
  |(line,ind) when (line = 0) -> c
  |_ -> raise Not_found

let update_right (c:cursor) (txtlist:string list) : cursor=
  let lst_len = (List.length txtlist)-1 in
  begin match c with 
    |(line,ind) -> let cur = list_get txtlist line 0 in
      let cur_end = (String.length cur)-1 in
      begin match c with 
        |(line,ind) when (ind < cur_end) -> update_cursor line (ind+1)
        |(line,ind) when (line < lst_len) -> update_cursor (line+1) 0
        |(line,ind) when (line = lst_len) -> c
        |_ -> raise Not_found end
  end

let update_up (c:cursor) (txtlist:string list) : cursor=
  match c with
  |(line,ind) when (line > 0) -> let prev = list_get txtlist (line-1) 0 in
    let prev_len = (String.length prev)-1 in
    begin if ind > prev_len then
        update_cursor (line-1) prev_len
      else update_cursor (line-1) ind end
  |(line,ind) when (line = 0) -> c
  |_ -> raise Not_found

let update_down (c:cursor) (txtlist:string list) : cursor=
  let lst_len = (List.length txtlist)-1 in
  begin match c with
    |(line,ind) when (line < lst_len) -> 
      let next = list_get txtlist (line+1) 0 in
      let next_len = (String.length next)-1 in
      begin if ind > next_len then
          update_cursor (line+1) next_len
        else update_cursor (line+1) ind end
    |(line,ind) when (line = lst_len) -> c
    |_ -> raise Not_found end