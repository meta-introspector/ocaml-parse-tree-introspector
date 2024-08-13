
module type Fields = sig
  type label
  type 'a loc
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
  type package_type
  type pattern
  type payload
  type rec_flag

  type structure
  type type_declaration
  type type_exception
  type type_extension
  type value_binding
  type value_binding_list
  type value_description

  type row_field = {
  prf_desc : row_field_desc;
  prf_loc : Location.t;
  prf_attributes : attributes;
}
                  [@@deriving  yojson]

and row_field_desc =
  | Rtag of label loc * bool * core_type list
      (** [Rtag(`A, b, l)] represents:
           - [`A]                   when [b] is [true]  and [l] is [[]],
           - [`A of T]              when [b] is [false] and [l] is [[T]],
           - [`A of T1 & .. & Tn]   when [b] is [false] and [l] is [[T1;...Tn]],
           - [`A of & T1 & .. & Tn] when [b] is [true]  and [l] is [[T1;...Tn]].

          - The [bool] field is true if the tag contains a
            constant (empty) constructor.
          - [&] occurs when several types are used for the same constructor
            (see 4.2 in the manual)
        *)
  | Rinherit of core_type  (** [[ | t ]] *)
                  [@@deriving  yojson]
end
