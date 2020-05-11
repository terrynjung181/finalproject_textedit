open Function
open Command
open State

(** [repeating file_name file_list c] take in user input for commands
    and returns the edited or saved/deleted file. This function is recursively
    called so that the user can constantly input commands and quits upon the
    command "quit"*)
let rec repeating (file_name : string) (file_list : string list) (c:cursor)=
  let helper com file_name file_list =
    match com with
    |Command.Quit -> ANSITerminal.(print_string [green] 
                                     "Thank you for using our text editor!\n");
      exit 0
    |Command.Save -> save file_name file_list;
      repeating file_name file_list c
    |Command.InsertText x -> let file_list = insert_text file_list x c in
      repeating file_name file_list c
    |Command.DeleteText x -> let file_list = delete_text file_list c in
      let c_new = update_left c file_list in
      repeating file_name file_list c_new
    |Command.DeleteFile -> delete file_name;
      ANSITerminal.(print_string [red] 
                      ("The file "^file_name^" has been deleted.\n"));
    |Command.Search x -> begin match search file_list x with
        |(a,b) -> let c_new = update_cursor a b in
          repeating file_name file_list c_new end
    |Command.Duplicate -> duplicate file_name;
      repeating file_name file_list c
    |Command.Help -> ANSITerminal.(print_string [magenta]"\"search (word)\": ");
      print_string (" Finds first occurrence of given (word).\n");
      ANSITerminal.(print_string [magenta] "\"insert_text (word)\": ");
      print_string ("Inserts (word) before the position of the cursor.\n");
      ANSITerminal.(print_string [magenta] "\"replace\": "); print_string 
        ("Replaces word in text file with another word of your choice.\n");
      ANSITerminal.(print_string [magenta] "\"cut_line\": ");
      print_string ("Deletes the entire line that cursor is currently on.\n");
      ANSITerminal.(print_string [magenta] "\"rename\": ");
      print_string ("Allows your file to be renamed.\n");
      ANSITerminal.(print_string [magenta] "\"duplicate\": ");
      print_string ("Makes a copy of your file in the same directory.\n");
      ANSITerminal.(print_string [magenta] "\"delete_file\": \n");
      print_string ("Deletes entire file from directory.");
      ANSITerminal.(print_string [magenta]  "\"del\": "); print_string 
        ("Deletes character that is before the current position of cursor\n");
      ANSITerminal.(print_string [magenta] "\"save\": ");
      print_string ("Saves all of the changes made to the file contents.\n");
      ANSITerminal.(print_string [magenta] "\"l\": ");
      print_string ("Moves cursor one character to the left.\n");
      ANSITerminal.(print_string [magenta] "\"r\": ");
      print_string ("Moves cursor one character to the right.\n");
      ANSITerminal.(print_string [magenta] "\"d\": ");
      print_string ("Moves cursor one character to the down.\n");
      ANSITerminal.(print_string [magenta] "\"u\": ");
      print_string ("Moves cursor one character to the up.\n");
      ANSITerminal.(print_string [magenta] "\"quit\": ");
      print_string ("Quits back to terminal.\n");
      print_endline " ";
      print_string "Press enter to continue.";
      let _ = read_line() in
      repeating file_name file_list c 
    |Command.Left -> let c_new = update_left c file_list in 
      repeating file_name file_list c_new
    |Command.Right -> let c_new = update_right c file_list in
      repeating file_name file_list c_new
    |Command.Up -> let c_new = update_up c file_list in
      repeating file_name file_list c_new
    |Command.Down -> let c_new = update_down c file_list in
      repeating file_name file_list c_new
    |Command.CutLine -> let length = List.length file_list in
      if (get_line c)=length then
        let c_new = update_up c file_list in
        let file_list = cut_line file_list c in
        repeating file_name file_list c_new
      else let file_list = cut_line file_list c in
        repeating file_name file_list c
    |Command.Rename x -> rename file_name x;
      repeating x file_list c
    |Command.Replace -> 
      ANSITerminal.(print_string [green] "Enter word to replace:\n");
      ANSITerminal.(print_string [green]  "> ");
      let word_old = read_line () in
      ANSITerminal.(print_string [green]"Enter word to replace with:\n");
      ANSITerminal.(print_string [green]  "> ");
      let word_new = read_line () in 
      let list_new = replace file_list word_old word_new in 
      begin match search file_list word_old with 
        |(a,b) -> let c_new = update_cursor a b in
          repeating file_name list_new c_new end in
  show file_list c 0;
  print_endline " ";
  ANSITerminal.(print_string[green]
                  "Please type command (type help for available commands)\n");
  print_string "> ";
  let user_ans = read_line () in
  print_endline " ";
  try helper (Command.parse user_ans) file_name file_list
  with
  |Command.Empty -> 
    ANSITerminal.(print_string[red] "Invalid command. Please try again. \n");
    repeating file_name file_list c
  |Command.Malformed -> 
    ANSITerminal.(print_string[red] "Invalid command. Please try again. \n");
    repeating file_name file_list c
  |Function.UnknownWord -> 
    ANSITerminal.(print_string[red] 
                    "Could not find word. Please try again. \n");
    repeating file_name file_list c

(** [start_editor file_name] starts the text editor in file [file_name]. *)
let start_editor file_name = 
  if Sys.file_exists file_name = true
  then let file_list = txt_to_list file_name in
    let cursor = update_cursor 0 0 in
    repeating file_name file_list cursor
  else let _ = create file_name in 
    repeating file_name [" "] (update_cursor 0 0)

(** [find_path dir_path path_list] returns a list of strings where each
    element is a directory. It takes a string of directory paths where each
    directory is divided by the character '/'. The last element is the file_name 
    that should be opened or created. *)
let rec find_path (dir_path : string) (path_list : string list)= 
  match String.index_opt dir_path '/' with
  |None -> dir_path :: path_list
  |Some x -> let length = String.length dir_path in
    let marker = x+1 in 
    let cur_dir = String.sub dir_path 0 marker in
    let next_dir = String.sub dir_path marker (length-marker) in
    find_path next_dir (cur_dir::path_list)

(** [change_dir dir_path] changes the current directory. The function takes in
    a list of strings. Each element is a directory name that can be moved into.
    The directories should be in order. If the directory does not exist,
    change_dir will throw a Sys error. *)
let rec change_dir (dir_path : string list)=
  match dir_path with
  |[] -> ()
  |h :: t -> Sys.chdir h; change_dir t

(** [extract_path path_list] returns the list of strings of directories only.
    This helper function takes the input from [find_path] and separates the
    last element(file name) and returns the rest. *)
let rec extract_path (path_list : string list) =
  match List.rev path_list with
  |[] -> []
  |h::t -> if (List.length path_list = 1) then []
    else h :: extract_path t

(** [extract_file path_list] returns only the name of the file. This helper
    function takes the input from [find_path] and separates the last element
    (file name) and only returns that. *)
let extract_file (path_list : string list) =
  match path_list with
  |[] -> ""
  |h::t -> h

(** [print_list list] prints on the elements of a list in order. This 
    function is used for debuggin and is a helper function.*)
let rec print_list list = 
  match list with
  |[] -> print_string "done"
  |h::t -> print_string h; print_list t

(** [main ()] prompts for the editor to appear for texting*)
let main () =
  ANSITerminal.(print_string [on_red] " "); (print_string "   ");
  ANSITerminal.(print_string [on_red] " "); (print_string " ");
  ANSITerminal.(print_string [on_yellow] "     "); (print_string " ");
  ANSITerminal.(print_string [on_green] " ");(print_string "      ");
  ANSITerminal.(print_string [on_cyan] "     ");(print_string "  ");
  ANSITerminal.(print_string [on_blue] "   ");(print_string "   ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string "  ");
  ANSITerminal.(print_string [on_white] "     "); (print_string " \n");
  ANSITerminal.(print_string [on_red] " "); (print_string "   ");
  ANSITerminal.(print_string [on_red] " "); (print_string " ");
  ANSITerminal.(print_string [on_yellow] " "); (print_string "     ");
  ANSITerminal.(print_string [on_green] " ");(print_string "      ");
  ANSITerminal.(print_string [on_cyan] " ");(print_string "     ");
  ANSITerminal.(print_string [on_blue] " ");(print_string "   ");
  ANSITerminal.(print_string [on_blue] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_white] " "); (print_string "\n");
  ANSITerminal.(print_string [on_red] " "); (print_string "   ");
  ANSITerminal.(print_string [on_red] " "); (print_string " ");
  ANSITerminal.(print_string [on_yellow] "     "); (print_string " ");
  ANSITerminal.(print_string [on_green] " ");(print_string "      ");
  ANSITerminal.(print_string [on_cyan] " ");(print_string "     ");
  ANSITerminal.(print_string [on_blue] " ");(print_string "   ");
  ANSITerminal.(print_string [on_blue] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_white] "     "); (print_string "\n");
  ANSITerminal.(print_string [on_red] " "); (print_string " ");
  ANSITerminal.(print_string [on_red] " "); (print_string " ");
  ANSITerminal.(print_string [on_red] " "); (print_string " ");
  ANSITerminal.(print_string [on_yellow] " "); (print_string "     ");
  ANSITerminal.(print_string [on_green] " ");(print_string "      ");
  ANSITerminal.(print_string [on_cyan] " ");(print_string "     ");
  ANSITerminal.(print_string [on_blue] " ");(print_string "   ");
  ANSITerminal.(print_string [on_blue] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_white] " "); (print_string " \n");
  print_string " "; ANSITerminal.(print_string [on_red] " ");
  print_string " "; ANSITerminal.(print_string [on_red] " "); print_string "  ";
  ANSITerminal.(print_string [on_yellow] "     "); (print_string " ");
  ANSITerminal.(print_string [on_green] "     ");(print_string "  ");
  ANSITerminal.(print_string [on_cyan] "     ");(print_string "  ");
  ANSITerminal.(print_string [on_blue] "   ");(print_string "  ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_magenta] " ");(print_string " ");
  ANSITerminal.(print_string [on_white] "     "); (print_string " \n");
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the text editor made by us.\n");
  print_endline "Please enter the path to file.\n";
  print_string  "> ";
  let path_list = find_path (read_line ()) [] in
  let path_dir = extract_path path_list in
  change_dir path_dir;
  match extract_file path_list with
  | exception End_of_file -> ()
  | file_name -> start_editor file_name

(* Execute the main function for the editor. *)
let () = main ()