module type AbstractStructure = sig
  type expression
  type attributes
  type rec_flag
  type value_binding
  type value_description
  type value_binding_list
  type type_declaration
  type type_extension
  type type_exception
  type module_binding
  type module_type_declaration
  type open_declaration
  type class_declaration
  type class_type_declaration 
  type include_declaration
  type attribute
  type extension
  
type  structure = structure_item list
[@@deriving  yojson]
and  structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }
[@@deriving  yojson]
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

end
