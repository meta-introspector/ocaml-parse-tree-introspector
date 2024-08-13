module type ModuleType = sig

  (** {1 Module language} *)
  (** {2 Type expressions for the module language} *)
  type override_flag
  type variance
  type class_description
  type module_expr
  type mutable_flag
  type private_flag
  type virtual_flag
  type 'a loc
  type label

  type constant
  type closed_flag
  type location_stack
  type object_field
  type arg_label
  type attribute
  type attributes
  type class_declaration
  type class_expr
  type class_type
  type class_type_declaration 
  type core_type
  type expression
  type extension

  type module_binding
  type module_exp


  type package_type
  type pattern
  type payload
  type rec_flag
  type row_field
  type structure
  type type_declaration
  type type_exception
  type type_extension
  type value_binding
  type value_binding_list
  type value_description

type module_type =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     pmty_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

[@@deriving  yojson]
and module_type_desc =
  | Pmty_ident of Longident.t loc  (** [Pmty_ident(S)] represents [S] *)
  | Pmty_signature of signature  (** [sig ... end] *)
  | Pmty_functor of functor_parameter * module_type
      (** [functor(X : MT1) -> MT2] *)
  | Pmty_with of module_type * with_constraint list  (** [MT with ...] *)
  | Pmty_typeof of module_expr  (** [module type of ME] *)
  | Pmty_extension of extension  (** [[%id]] *)
  | Pmty_alias of Longident.t loc  (** [(module M)] *)

[@@deriving  yojson]
and functor_parameter =
  | Unit  (** [()] *)
  | Named of string option loc * module_type
      (** [Named(name, MT)] represents:
            - [(X : MT)] when [name] is [Some X],
            - [(_ : MT)] when [name] is [None] *)

[@@deriving  yojson]
and signature = signature_item list

[@@deriving  yojson]
and signature_item =
    {
     psig_desc: signature_item_desc;
     psig_loc: Location.t;
    }

[@@deriving  yojson]
and signature_item_desc =
  | Psig_value of value_description
      (** - [val x: T]
            - [external x: T = "s1" ... "sn"]
         *)
  | Psig_type of rec_flag * type_declaration list
      (** [type t1 = ... [@@deriving  yojson]
and ... and tn  = ...] *)
  | Psig_typesubst of type_declaration list
      (** [type t1 := ... and ... and tn := ...]  *)
  | Psig_typext of type_extension  (** [type t1 += ...] *)
  | Psig_exception of type_exception  (** [exception C of T] *)
  | Psig_module of module_declaration  (** [module X = M] and [module X : MT] *)
  | Psig_modsubst of module_substitution  (** [module X := M] *)
  | Psig_recmodule of module_declaration list
      (** [module rec X1 : MT1 and ... and Xn : MTn] *)
  | Psig_modtype of module_type_declaration
      (** [module type S = MT] and [module type S] *)
  | Psig_modtypesubst of module_type_declaration
      (** [module type S :=  ...]  *)
  | Psig_open of open_description  (** [open X] *)
  | Psig_include of include_description  (** [include MT] *)
  | Psig_class of class_description list
      (** [class c1 : ... [@@deriving  yojson]
and ... and cn : ...] *)
  | Psig_class_type of class_type_declaration list
      (** [class type ct1 = ... and ... and ctn = ...] *)
  | Psig_attribute of attribute  (** [[\@\@\@id]] *)
  | Psig_extension of extension * attributes  (** [[%%id]] *)

[@@deriving  yojson]
and module_declaration =
    {
     pmd_name: string option loc;
     pmd_type: module_type;
     pmd_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pmd_loc: Location.t;
    }
(** Values of type [module_declaration] represents [S : MT] *)
[@@deriving  yojson]
and module_substitution =
    {
     pms_name: string loc;
     pms_manifest: Longident.t loc;
     pms_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pms_loc: Location.t;
    }
(** Values of type [module_substitution] represents [S := M] *)

[@@deriving  yojson]
and module_type_declaration =
    {
     pmtd_name: string loc;
     pmtd_type: module_type option;
     pmtd_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pmtd_loc: Location.t;
    }
(** Values of type [module_type_declaration] represents:
   - [S = MT],
   - [S] for abstract module type declaration,
     when {{!module_type_declaration.pmtd_type}[pmtd_type]} is [None].
*)

and 'a open_infos =
    {
     popen_expr: 'a;
     popen_override: override_flag;
     popen_loc: Location.t;
     popen_attributes: attributes;
    }
(** Values of type ['a open_infos] represents:
    - [open! X] when {{!open_infos.popen_override}[popen_override]}
                  is {{!Asttypes.override_flag.Override}[Override]}
    (silences the "used identifier shadowing" warning)
    - [open  X] when {{!open_infos.popen_override}[popen_override]}
                  is {{!Asttypes.override_flag.Fresh}[Fresh]}
*)
[@@deriving  yojson]
and  open_description = Longident.t loc open_infos
(** Values of type [open_description] represents:
    - [open M.N]
    - [open M(N).O] *)
[@@deriving  yojson]
and  open_declaration = module_expr open_infos
(** Values of type [open_declaration] represents:
    - [open M.N]
    - [open M(N).O]
    - [open struct ... end] *)
[@@deriving  yojson]
and  'a include_infos =
    {
     pincl_mod: 'a;
     pincl_loc: Location.t;
     pincl_attributes: attributes;
    }
[@@deriving  yojson]
and  include_description = module_type include_infos
(** Values of type [include_description] represents [include MT] *)
[@@deriving  yojson]
and  include_declaration = module_expr include_infos
(** Values of type [include_declaration] represents [include ME] *)
[@@deriving  yojson]
and  with_constraint =
  | Pwith_type of Longident.t loc * type_declaration
      (** [with type X.t = ...
]
            Note: the last component of the longident must match
            the name of the type_declaration. *)
  | Pwith_module of Longident.t loc * Longident.t loc
      (** [with module X.Y = Z] *)
  | Pwith_modtype of Longident.t loc * module_type
      (** [with module type X.Y = Z] *)
  | Pwith_modtypesubst of Longident.t loc * module_type
      (** [with module type X.Y := sig end] *)
  | Pwith_typesubst of Longident.t loc * type_declaration
      (** [with type X.t := ..., same format as [Pwith_type]] *)
  | Pwith_modsubst of Longident.t loc * Longident.t loc
      (** [with module X.Y := Z] *)
end
