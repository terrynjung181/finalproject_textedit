open State
open Command

exception UnknownWord
exception FileError
(* This is the main search function.*)

(** [upd_txtlist_no_space txtlist] replaces the " " lines of the
    file content with empty strings.*)
let rec upd_txtlist_no_space txtlist =
  match txtlist with
  |[] -> []
  |h :: t -> if h = " " then "" :: (upd_txtlist_no_space t)
    else h :: (upd_txtlist_no_space t)

let save file_name string_list= 
  let new_str_lst = upd_txtlist_no_space string_list in
  let file_open = open_out (file_name) in
  let rec save_lines str_lst=
    match str_lst with
    | [] -> close_out file_open
    | h::t -> output_string file_open h;
      output_string file_open "
";
      save_lines t in
  save_lines new_str_lst;
  close_out file_open

let delete file_name =
  Sys.remove file_name

let create (file_name:string) =
  match open_out file_name with
  |x -> x
  |exception Sys_error _-> raise FileError

let rename file_name new_file_name=
  Sys.rename file_name new_file_name

(** [upd_txtlist_space txtlist] replaces the empty string lines
    of the file content with " ".*)
let rec upd_txtlist_space txtlist =
  match txtlist with
  |[] -> []
  |h :: t -> if h = "" then " " :: (upd_txtlist_space t)
    else h :: (upd_txtlist_space t)

let txt_to_list file_name =
  let file_open = open_in file_name in
  let read_line ()= try Some(input_line file_open) with
      End_of_file -> None in
  let rec read_full acc = 
    match read_line() with
    |Some x -> read_full (x::acc)
    |None -> close_in file_open; List.rev acc in
  upd_txtlist_space (read_full [])

let rec duplicate (file_duplicate:string) =
  if Sys.file_exists file_duplicate = true then
    let file_name = file_duplicate^"_copy" in
    let string_list = txt_to_list file_duplicate in
    let file_copy = create file_name in
    close_out file_copy;
    save file_name string_list
  else ()

let cut_line (txtlist : string list) (c:cursor)=
  let rec cut_helper (txtlist) (line) (count) =
    match txtlist with 
    |[] -> []
    |h::t -> if count = line then t
      else h::(cut_helper t line (count+1)) in
  let line = get_line c in
  cut_helper txtlist line 0

(**[helper_search line_string word] returns the tuple that indicates 
 * the position index of the first character of [word] in [line_string]. *)
let helper_search line_string word =
  let first_char = String.get word 0 in 
  let f_char_index = String.index_opt line_string first_char in
  let rec loop f_char_index=
    match f_char_index with
    |None -> raise UnknownWord
    |Some x -> let ind_end = String.length word in 
      begin match String.sub line_string x ind_end with
        |check -> if check = word then x
          else loop(String.index_from_opt line_string (x+1) first_char)
        |exception Invalid_argument _ -> raise UnknownWord end in
  loop f_char_index

let search txt_list word =
  let rec help_search content word counter=
    try
      match content with
      |[] -> raise UnknownWord
      |h::t -> (counter, helper_search 
                  (String.lowercase_ascii h) (String.lowercase_ascii word))
    with UnknownWord -> 
    match content with
    |[] -> raise UnknownWord
    |h :: t -> help_search t word (counter+1) in
  help_search txt_list word 0;;

(**[replace_helper cur_line word_old word_new num] returns the new nth 
   [cur_line] line after the old word [word_old] is replaced with the new word 
   [word_new] that has a length of [num] *)
let rec replace_helper cur_line word_old word_new num =
  let length_old = String.length word_old in 
  let length_full = String.length cur_line in
  let first = String.sub cur_line 0 num in
  let length_first = String.length first in
  let length_second = length_full - length_old - length_first in
  let num_second = num + length_old in
  let second = String.sub cur_line num_second length_second in
  first ^ word_new ^ second;;

(**[loop_replace content word_old word_new line num count] returns the new 
   string list with the first appearence of the targeted word [word_old] 
   replaced by new word [word_new]. *)
let rec loop_replace content word_old word_new line num count = 
  match content with
  |[] -> raise UnknownWord
  |h :: t -> if count = line then
      (replace_helper h word_old word_new num) :: t
    else let count = count + 1 in 
      h :: loop_replace t word_old word_new line num count;;

let replace (txt_list:string list) (word_old:string) (word_new:string) =
  try let ind = search txt_list word_old in
    match ind with
    |(a,b) -> loop_replace txt_list word_old word_new a b 0
  with UnknownWord -> raise UnknownWord;;

(** [get_prev file_list line count] returns the string line that comes 
    before the given line value. This function is used as a helper 
    in delete_text.*)
let rec get_prev file_list line count =
  match file_list with
  |[] -> raise Not_found
  |h::t -> if line-1 = count then h
    else get_prev t line (count+1)

(** [del_loop file_list count prev line ind] returns a string list and
    with a character deleted with the given line and ind. This funciton
    is used as a helper in delete_text.*)
let rec del_loop file_list count prev line ind=
  match file_list with
  |[] -> []
  |h::t -> if line = count then
      if ind = 0 then (prev^h)::t
      else let len = String.length h in
        let first = String.sub h 0 (ind-1) in
        let second = String.sub h (ind) (len-ind) in
        (first^second) :: t
    else h::del_loop t (count+1) prev line ind

let delete_text (file_list:string list) (c:cursor) =
  let line = get_line c in 
  let ind = get_ind c in
  if (line=0) && (ind=0) then file_list
  else if ind = 0 then 
    let prev = get_prev file_list line 0 in 
    let list_new = del_loop file_list 0 prev line ind in
    cut_line list_new (update_cursor (line-1) 0)
  else del_loop file_list 0 "" line ind 

let insert_text (txt_list:string list) (word_insert:string) (c:cursor) =
  let line = get_line c in
  let ind = get_ind c in
  let insert_helper_ind t ind =
    let first_part = String.sub t 0 ind in
    let second_part = String.sub t ind ((String.length t)-ind) in
    first_part^word_insert^second_part in
  let rec insert_helper_line (txt_list) (line) (count) =
    match txt_list with
    | [] -> []
    |h::t -> if count = line then (insert_helper_ind h ind)::t
      else h::(insert_helper_line t line (count+1)) in
  insert_helper_line txt_list line 0 

