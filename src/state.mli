(** A cursor is a tuple of two integers. The first integer represents
    line number the cursor is on. The second integer represents the index
    the cursor is on in the correpsonding line.*)
type cursor

(** [update_cursor line ind] updates current cursor by returning new
    cursor with new line and index values*)
val update_cursor : int -> int -> cursor

(** [get_line c] returns the line number the cursor is currently on*)
val get_line : cursor -> int

(** [get_ind c] returns the index number the cursor is currently on*)
val get_ind : cursor -> int

(** [show txtlist c count] prints out the current file in the terminal. It
    shows where the cursor is by highlighting the corresponding character. *)
val show : string list -> cursor -> int -> unit

(** [update_left c txtlist] updates the index of the cursor to the left. 
    If the cursor is not on the first index of the current line, it
    moves one index to the left. Otherwise, it moves up a line. 
    If the cursor is on the first index of the first line, the cursor
    does not change.*)
val update_left : cursor -> string list -> cursor

(** [update_right c txtlist] updates the index of the cursor to the right. 
    If the cursor is not on the last index of the current line, it
    moves one index to the right. Otherwise, it moves down a line. 
    If the cursor is on the last index of the last line, the cursor
    does not change.*)
val update_right : cursor -> string list -> cursor

(** [update_up c txtlist] updates the line of the index to one up. If
    the cursor is not on the first line of the file, it updates the 
    cursor to move up one. Otherwise, the cursor does not change.It 
    maintains the index if the next line is equal to or greater than 
    the current line length. *)
val update_up : cursor -> string list -> cursor

(** [update_down c txtlist] updates the line of the index to one down. If
    the cursor is not on the last line of the file, it updates the 
    cursor to move down one. Otherwise, the cursor does not change. It 
    maintains the index if the next line is equal to or greater than 
    the current line length.*)
val update_down : cursor -> string list -> cursor