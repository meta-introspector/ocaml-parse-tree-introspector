module type PackageType = sig
    type 'a loc
  type open_description
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
  type include_declaration
  type location_stack
  type module_binding
  type module_exp
  type module_type
  type module_type_declaration
  type open_declaration
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

type package_type = Longident.t loc * (Longident.t loc * core_type) list
(** As {!package_type} typed values:
         - [(S, [])] represents [(module S)],
         - [(S, [(t1, T1) ; ... ; (tn, Tn)])]
          represents [(module S with type t1 = T1 and ... and tn = Tn)].
       *)
                                       [@@deriving  yojson]
end
