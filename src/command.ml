type command =
  |Save
  |InsertText of string
  |DeleteText of string
  |DeleteFile
  |Duplicate 
  |Help
  |Quit
  |Search of string
  |Left
  |Right
  |Up
  |Down
  |CutLine
  |Rename of string
  |Replace

exception Empty
exception Malformed

let parse_ins_del str =
  if str = "" then raise Empty
  else let no_white = String.trim str in
    let str_length = String.length str in
    try
      let com_space = String.index no_white ' ' in
      let com = String.sub no_white 0 (com_space) in
      let com_length = String.length com in
      let com_first = String.get com 0 in
      let next_space = (String.index str com_first) + com_length + 1 in
      let next_length = str_length - next_space in
      let next = String.sub str next_space next_length in
      (com,next)
    with Not_found -> (no_white, "")

let parse str =
  match parse_ins_del str with
  |(a,b) when (a = "insert_text") -> InsertText b
  |(a,b) when (a = "del") -> DeleteText b
  |(a,b) when (a = "delete_file") -> DeleteFile
  |(a,b) when (a = "search") -> Search b
  |(a,b) when (a = "duplicate") -> Duplicate
  |(a,b) when (a = "help") -> Help
  |(a,b) when (a = "save") -> Save
  |(a,b) when (a = "quit") -> Quit
  |(a,b) when (a = "l") -> Left
  |(a,b) when (a = "u") -> Up
  |(a,b) when (a = "d") -> Down
  |(a,b) when (a = "r") -> Right
  |(a,b) when (a = "cut_line") -> CutLine
  |(a,b) when (a = "rename") -> Rename b
  |(a,b) when (a = "replace") -> Replace
  |exception Empty -> raise Empty
  |_ -> raise Malformed
