module type ModuleValue = sig
  type 'a loc
  type attribute
  type functor_parameter
  type attributes
  type class_declaration
  type class_expr
  type class_type
  type class_type_declaration 
  type core_type
  type expression
  type extension
  type include_declaration
  type location_stack
  type module_exp
  type module_type
  type module_type_declaration
  type open_declaration
  type package_type
  type pattern
  type payload
  type rec_flag
  type row_field
  type type_declaration
  type type_exception
  type type_extension
  type value_binding_list
  type value_description

  (** {2 Value expressions for the module language} *)

type  module_expr =
    {
     pmod_desc: module_expr_desc;
     pmod_loc: Location.t;
     pmod_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }
[@@deriving  yojson]
and  module_expr_desc =
  | Pmod_ident of Longident.t loc  (** [X] *)
  | Pmod_structure of structure  (** [struct ... end] *)
  | Pmod_functor of functor_parameter * module_expr
      (** [functor(X : MT1) -> ME] *)
  | Pmod_apply of module_expr * module_expr  (** [ME1(ME2)] *)
  | Pmod_constraint of module_expr * module_type  (** [(ME : MT)] *)
  | Pmod_unpack of expression  (** [(val E)] *)
  | Pmod_extension of extension  (** [[%id]] *)
                        [@@deriving  yojson]

(*moved to structure.ml*)
and  structure = structure_item list
                   [@@deriving  yojson]

(*moved to structure.ml*)
and  structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }
      [@@deriving  yojson]

(*moved to structure.ml*)
and  structure_item_desc =
  | Pstr_eval of expression * attributes  (** [E] *)
  | Pstr_value of rec_flag * value_binding list
      (** [Pstr_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
            - [let P1 = E1 and ... and Pn = EN]
                when [rec] is {{!Asttypes.rec_flag.Nonrecursive}[Nonrecursive]},
            - [let rec P1 = E1 and ... and Pn = EN ]
                when [rec] is {{!Asttypes.rec_flag.Recursive}[Recursive]}.
        *)
  | Pstr_primitive of value_description
      (** - [val x: T]
            - [external x: T = "s1" ... "sn" ]*)
  | Pstr_type of rec_flag * type_declaration list
      (** [type t1 = ... and ... and tn = ...] *)
  | Pstr_typext of type_extension  (** [type t1 += ...] *)
  | Pstr_exception of type_exception
      (** - [exception C of T]
            - [exception C = M.X] *)
  | Pstr_module of module_binding  (** [module X = ME] *)
  | Pstr_recmodule of module_binding list
      (** [module rec X1 = ME1 and ... and Xn = MEn] *)
  | Pstr_modtype of module_type_declaration  (** [module type S = MT] *)
  | Pstr_open of open_declaration  (** [open X] *)
  | Pstr_class of class_declaration list
      (** [class c1 = ... and ... and cn = ...] *)
  | Pstr_class_type of class_type_declaration list
      (** [class type ct1 = ... and ... and ctn = ...] *)
  | Pstr_include of include_declaration  (** [include ME] *)
  | Pstr_attribute of attribute  (** [[\@\@\@id]] *)
  | Pstr_extension of extension * attributes  (** [[%%id]] *)
                                    [@@deriving  yojson]


and  value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_attributes: attributes;
    pvb_loc: Location.t;
  }
[@@deriving  yojson]
and  module_binding =
    {
     pmb_name: string option loc;
     pmb_expr: module_expr;
     pmb_attributes: attributes;
     pmb_loc: Location.t;
    }
(** Values of type [module_binding] represents [module X = ME] *)
end
