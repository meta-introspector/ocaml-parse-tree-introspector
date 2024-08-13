(** {2 Value descriptions} *)
module type ValueDescription = sig
  type 'a loc
  type 'a class_infos
  type private_flag
  type mutable_flag
  type label
  type override_flag
  type open_description
  type arg_label
  type attribute
  type attributes
  type class_type
  type class_type_declaration 
  type core_type
  type expression
  type extension
  type include_declaration
  type location_stack
  type module_binding
  type module_exp
  type module_type
  type module_type_declaration
  type open_declaration
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


type value_description =
    {
     pval_name: string loc;
     pval_type: core_type;
     pval_prim: string list;
     pval_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
     pval_loc: Location.t;
    }
[@@deriving  yojson]
(** Values of type {!value_description} represents:
    - [val x: T],
            when {{!value_description.pval_prim}[pval_prim]} is [[]]
    - [external x: T = "s1" ... "sn"]
            when {{!value_description.pval_prim}[pval_prim]} is [["s1";..."sn"]]
*)
end
