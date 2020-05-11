(** 
   Representation of static search function data.

   This module represents the data stored in function files.
    It handles loading of the data the text file
   searching in the specific text that is converted into string.
*)

open State

(** The abstract type of values representing adventures. *)

(** Raised when word can not be found. *)
exception UnknownWord

(** Raised when there is an error with creating the new file. *)
exception FileError

(** [save file_name] is the file saved from using the [file_name]*)
val save : string -> string list -> unit

(** [delete file_name] is the file deleted from using the [file_name]*)
val delete : string -> unit

(** [create file_name] is the new file named [file_name].*)
val create : string -> out_channel

(** [rename file_name new_file_name] is the file renamed from using the 
    [file_name] and [new_file_name]*)
val rename : string -> string -> unit

(** [duplicate file_duplicate] is the duplicate of the file named
    [file_duplicate]*)
val duplicate : string -> unit

(**[search file_name word] returns the line number of [word] in [file_name] 
   and the position index of the first character of [word] in that line *)
val search : string list -> string -> int*int

(**[replace file_name word_old word_new] returns the new text from file 
   [file_name:string] by replacing the first appearence of the word [word_old] 
   with a new word [word_new].*)
val replace : string list -> string -> string -> string list

(**[delete_text file_name word_delete] returns the new text from the file 
   [file_name:string] with [word_delete] deleted. *)
(* val delete_text : string -> string -> string list *)

(**[insert_text file_name word_insert word_target] returns the new text from 
   the file [file_name:string] with [word_insert] inserted before 
   [word_target] *)
val insert_text : string list -> string -> State.cursor -> string list

(** [delete_text file_name c] removes the character before the placement
    of the cursor [c] and returns the updated string list.*)
val delete_text : string list -> State.cursor -> string list

(** [txt_to_list file_name] returns a string list where each element in the 
    list is a line from the [file_name] *)
val txt_to_list : string -> string list

(** [cut_line txtlist c] returns the txtlist with the line that the cursor is
    currently placed on removed.*)
val cut_line : string list -> State.cursor ->string list