module type ObjectField = sig
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
  type value_description

type object_field = {
  pof_desc : object_field_desc;
  pof_loc : Location.t;
  pof_attributes : attributes;
}
                     [@@deriving  yojson]

and object_field_desc =
  | Otag of label loc * core_type
  | Oinherit of core_type
end
