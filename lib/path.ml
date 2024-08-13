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

(* Access paths *)
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
    Pident of Ident.t
  | Pdot of t * string
  | Papply of t * t
                    [@@deriving  yojson]
type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string
                  [@@deriving  yojson]
(* module Map : Map.S with type key = t *)
(* module Set : Set.S with type elt = t *)
