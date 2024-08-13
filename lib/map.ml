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
module type OrderedType =
  sig
    type t
           [@@deriving  yojson]
  end

module type S =
  sig
    type key
                      [@@deriving  yojson]
    type 'a t
                      [@@deriving  yojson]
  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t
                  [@@deriving  yojson]
    type 'a t =
        Empty
      | Node of {l:'a t; v:key; d:'a; r:'a t; h:int}
                  [@@deriving  yojson]

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration
                  [@@deriving  yojson]

end
