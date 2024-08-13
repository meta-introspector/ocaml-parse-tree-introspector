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

type t =
  | Local of { name: string; stamp: int }
  | Scoped of { name: string; stamp: int; scope: int }
  | Global of string
  | Predef of { name: string; stamp: int }
[@@deriving  yojson]
      (* the stamp is here only for fast comparison, but the name of
         predefined identifiers is always unique. *)

type 'a tbl =
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int
                                          [@@deriving  yojson]

and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }
[@@deriving  yojson]

include Identifiable.Make (struct
            type nonrec t = t
            [@@deriving  yojson]
  (* let compare = compare *)
  (* let output = output *)
  (* let print = print *)
  (* let hash = hash *)
  (* let equal = same *)
end)
(* let equal = original_equal *)
