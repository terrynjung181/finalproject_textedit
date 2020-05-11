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

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(**[parse_ins_del str] returns a tuple of two strings. The key of the tuple
   is the command that is read from the user input. The value is any thing
   that comes after the space after the command word.*)
val parse_ins_del : string -> string*string

(** [parse str] returns a type command with its corresponding string,
    if the command is of type string. The command is pulled out from
    parsing through the user input*)
val parse : string -> command