(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
(** The run-time library for lexers generated by [ocamllex]. *)

(** {1 Positions} *)

type position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
  }
[@@deriving  yojson]
(** A value of type [position] describes a point in a source file.
   [pos_fname] is the file name; [pos_lnum] is the line number;
   [pos_bol] is the offset of the beginning of the line (number
   of characters between the beginning of the lexbuf and the beginning
   of the line); [pos_cnum] is the offset of the position (number of
   characters between the beginning of the lexbuf and the position).
   The difference between [pos_cnum] and [pos_bol] is the character
   offset within the line (i.e. the column number, assuming each
   character is one column wide).

   See the documentation of type [lexbuf] for information about
   how the lexing engine will manage positions.
 *)

type lexbuf =
  { refill_buff : lexbuf -> unit;
    mutable lex_buffer : bytes;
    mutable lex_buffer_len : int;
    mutable lex_abs_pos : int;
    mutable lex_start_pos : int;
    mutable lex_curr_pos : int;
    mutable lex_last_pos : int;
    mutable lex_last_action : int;
    mutable lex_eof_reached : bool;
    mutable lex_mem : int array;
    mutable lex_start_p : position;
    mutable lex_curr_p : position;
  }
[@@deriving  yojson]
(** The type of lexer buffers. A lexer buffer is the argument passed
   to the scanning functions defined by the generated scanners.
   The lexer buffer holds the current state of the scanner, plus
   a function to refill the buffer from the input.

   Lexers can optionally maintain the [lex_curr_p] and [lex_start_p]
   position fields.  This "position tracking" mode is the default, and
   it corresponds to passing [~with_position:true] to functions that
   create lexer buffers. In this mode, the lexing engine and lexer
   actions are co-responsible for properly updating the position
   fields, as described in the next paragraph.  When the mode is
   explicitly disabled (with [~with_position:false]), the lexing
   engine will not touch the position fields and the lexer actions
   should be careful not to do it either; the [lex_curr_p] and
   [lex_start_p] field will then always hold the [dummy_pos] invalid
   position.  Not tracking positions avoids allocations and memory
   writes and can significantly improve the performance of the lexer
   in contexts where [lex_start_p] and [lex_curr_p] are not needed.

   Position tracking mode works as follows.  At each token, the lexing
   engine will copy [lex_curr_p] to [lex_start_p], then change the
   [pos_cnum] field of [lex_curr_p] by updating it with the number of
   characters read since the start of the [lexbuf].  The other fields
   are left unchanged by the lexing engine.  In order to keep them
   accurate, they must be initialised before the first use of the
   lexbuf, and updated by the relevant lexer actions (i.e. at each end
   of line -- see also [new_line]).
*)

type lex_tables =
  { lex_base : string;
    lex_backtrk : string;
    lex_default : string;
    lex_trans : string;
    lex_check : string;
    lex_base_code : string;
    lex_backtrk_code : string;
    lex_default_code : string;
    lex_trans_code : string;
    lex_check_code : string;
    lex_code: string;}
[@@deriving  yojson]
