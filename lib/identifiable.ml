(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type Thing = sig
  type t
  [@@deriving  yojson]
  (* include Hashtbl.HashedType with type t := t *)
  (* include Map.OrderedType with type t := t *)

end

module type Set = sig
  module T : Set.OrderedType
  include Set.S
          with type elt = T.t
     and type t = Set.Make (T).t
  [@@deriving  yojson]
end

module type Map = sig
  module T : Map.OrderedType
  include Map.S
    with type key = T.t
     and type 'a t = 'a Map.Make (T).t
end

module type Tbl = sig
  module T : sig
    type t
      [@@deriving  yojson]
    include Map.OrderedType with type t := t
    include Hashtbl.HashedType with type t := t
  end
  include Hashtbl.S
    with type key = T.t
     and type 'a t = 'a Hashtbl.Make (T).t

end

module Pair (A : Thing) (B : Thing) : Thing with type t = A.t * B.t = struct
  type t = A.t * B.t
  [@@deriving  yojson]
end

module Make_map (T : Thing) = struct
  (* include Map.Make (T) *)
  (* module T_set = Set.Make (T) *)
end

module Make_set (T : Thing) = struct
  (* include Set.Make (T) *)

end

module Make_tbl (T : Thing) = struct
  (* include Hashtbl.Make (T) *)

  (* module T_map = Make_map (T) *)
end

module type S = sig
  type t
[@@deriving  yojson]
  (* module T : Thing with type t = t *)
  (* include Thing with type t := T.t *)

  (* module Set : Set with module T := T *)
  (* module Map : Map with module T := T *)
  (* module Tbl : Tbl with module T := T *)
end

module Make (T : Thing) = struct
  (* module T = T *)
  (* include T *)
  (* module Set = Make_set (T) *)
  (* module Map = Make_map (T) *)
  (* module Tbl = Make_tbl (T) *)
end
