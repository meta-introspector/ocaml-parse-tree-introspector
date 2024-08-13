(** {2 Type declarations} *)
module type TypeDeclarations = sig
  type 'a class_infos
  type 'a loc
  type arg_label
  type attribute
  type attributes
  type class_description
  type class_type
  type class_type_declaration 
  type core_type
  type expression
  type extension
  type include_declaration
  type injectivity
  type label
  type location_stack
  type module_binding
  type module_exp
  type module_expr
  type module_type
  type module_type_declaration
  type mutable_flag
  type open_declaration
  type open_description
  type override_flag
  type package_type
  type pattern
  type payload
  type private_flag
  type rec_flag
  type row_field
  type structure
  type value_binding
  type value_binding_list
  type variance
  type virtual_flag

type type_declaration =
    {
     ptype_name: string loc;
     ptype_params: (core_type * (variance * injectivity)) list;
      (** [('a1,...'an) t] *)
     ptype_cstrs: (core_type * core_type * Location.t) list;
      (** [... constraint T1=T1'  ... constraint Tn=Tn'] *)
     ptype_kind: type_kind;
     ptype_private: private_flag;  (** for [= private ...] *)
     ptype_manifest: core_type option;  (** represents [= T] *)
     ptype_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     ptype_loc: Location.t;
    }
[@@deriving  yojson]
(**
   Here are type declarations and their representation,
   for various {{!type_declaration.ptype_kind}[ptype_kind]}
           and {{!type_declaration.ptype_manifest}[ptype_manifest]} values:
 - [type t]   when [type_kind] is {{!type_kind.Ptype_abstract}[Ptype_abstract]},
               and [manifest]  is [None],
 - [type t = T0]
              when [type_kind] is {{!type_kind.Ptype_abstract}[Ptype_abstract]},
               and [manifest]  is [Some T0],
 - [type t = C of T | ...]
              when [type_kind] is {{!type_kind.Ptype_variant}[Ptype_variant]},
               and [manifest]  is [None],
 - [type t = T0 = C of T | ...]
              when [type_kind] is {{!type_kind.Ptype_variant}[Ptype_variant]},
               and [manifest]  is [Some T0],
 - [type t = {l: T; ...}]
              when [type_kind] is {{!type_kind.Ptype_record}[Ptype_record]},
               and [manifest]  is [None],
 - [type t = T0 = {l : T; ...}]
              when [type_kind] is {{!type_kind.Ptype_record}[Ptype_record]},
               and [manifest]  is [Some T0],
 - [type t = ..]
              when [type_kind] is {{!type_kind.Ptype_open}[Ptype_open]},
               and [manifest]  is [None].
*)


and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list  (** Invariant: non-empty list *)
  | Ptype_open
[@@deriving  yojson]

and label_declaration =
    {
     pld_name: string loc;
     pld_mutable: mutable_flag;
     pld_type: core_type;
     pld_loc: Location.t;
     pld_attributes: attributes;  (** [l : T [\@id1] [\@id2]] *)
    }
[@@deriving  yojson]
(**
   - [{ ...; l: T; ... }]
                           when {{!label_declaration.pld_mutable}[pld_mutable]}
                             is {{!Asttypes.mutable_flag.Immutable}[Immutable]},
   - [{ ...; mutable l: T; ... }]
                           when {{!label_declaration.pld_mutable}[pld_mutable]}
                             is {{!Asttypes.mutable_flag.Mutable}[Mutable]}.

   Note: [T] can be a {{!core_type_desc.Ptyp_poly}[Ptyp_poly]}.
*)


and constructor_declaration =
    {
     pcd_name: string loc;
     pcd_vars: string loc list;
     pcd_args: constructor_arguments;
     pcd_res: core_type option;
     pcd_loc: Location.t;
     pcd_attributes: attributes;  (** [C of ... [\@id1] [\@id2]] *)
    }

[@@deriving  yojson]
and constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list
      (** Values of type {!constructor_declaration}
    represents the constructor arguments of:
  - [C of T1 * ... * Tn]     when [res = None],
                              and [args = Pcstr_tuple [T1; ... ; Tn]],
  - [C: T0]                  when [res = Some T0],
                              and [args = Pcstr_tuple []],
  - [C: T1 * ... * Tn -> T0] when [res = Some T0],
                              and [args = Pcstr_tuple [T1; ... ; Tn]],
  - [C of {...}]             when [res = None],
                              and [args = Pcstr_record [...]],
  - [C: {...} -> T0]         when [res = Some T0],
                              and [args = Pcstr_record [...]].
*)

[@@deriving  yojson]
and type_extension =
    {
     ptyext_path: Longident.t loc;
     ptyext_params: (core_type * (variance * injectivity)) list;
     ptyext_constructors: extension_constructor list;
     ptyext_private: private_flag;
     ptyext_loc: Location.t;
     ptyext_attributes: attributes;  (** ... [\@\@id1] [\@\@id2] *)
    }
(**
   Definition of new extensions constructors for the extensive sum type [t]
   ([type t += ...]).
*)

[@@deriving  yojson]
and extension_constructor =
    {
     pext_name: string loc;
     pext_kind: extension_constructor_kind;
     pext_loc: Location.t;
     pext_attributes: attributes;  (** [C of ... [\@id1] [\@id2]] *)
   }

[@@deriving  yojson]
and type_exception =
  {
    ptyexn_constructor : extension_constructor;
    ptyexn_loc : Location.t;
    ptyexn_attributes : attributes;  (** [... [\@\@id1] [\@\@id2]] *)
  }
(** Definition of a new exception ([exception E]). *)

[@@deriving  yojson]
and extension_constructor_kind =
  | Pext_decl of string loc list * constructor_arguments * core_type option
      (** [Pext_decl(existentials, c_args, t_opt)]
          describes a new extension constructor. It can be:
          - [C of T1 * ... * Tn] when:
               {ul {- [existentials] is [[]],}
                   {- [c_args] is [[T1; ...; Tn]],}
                   {- [t_opt] is [None]}.}
          - [C: T0] when
               {ul {- [existentials] is [[]],}
                   {- [c_args] is [[]],}
                   {- [t_opt] is [Some T0].}}
          - [C: T1 * ... * Tn -> T0] when
               {ul {- [existentials] is [[]],}
                   {- [c_args] is [[T1; ...; Tn]],}
                   {- [t_opt] is [Some T0].}}
          - [C: 'a... . T1 * ... * Tn -> T0] when
               {ul {- [existentials] is [['a;...]],}
                   {- [c_args] is [[T1; ... ; Tn]],}
                   {- [t_opt] is [Some T0].}}
       *)
  | Pext_rebind of Longident.t loc
  (** [Pext_rebind(D)] re-export the constructor [D] with the new name [C] *)
end
