module type ClassTypeExpression = sig
  (** {2 Type expressions for the class language} *)
  type variance
  type injectivity
  type mutable_flag
  type private_flag
  type virtual_flag
  type 'a loc
  type label
  type open_description
  type constant
  type closed_flag
  type location_stack
  type object_field
  type arg_label
  type attribute
  type attributes
  type class_declaration
  type class_expr

  type core_type
  type expression
  type extension
  type include_declaration
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

type class_type =
    {
     pcty_desc: class_type_desc;
     pcty_loc: Location.t;
     pcty_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

[@@deriving  yojson]
and class_type_desc =
  | Pcty_constr of Longident.t loc * core_type list
      (** - [c]
            - [['a1, ..., 'an] c] *)
  | Pcty_signature of class_signature  (** [object ... end] *)
  | Pcty_arrow of arg_label * core_type * class_type
      (** [Pcty_arrow(lbl, T, CT)] represents:
            - [T -> CT]
                     when [lbl] is {{!Asttypes.arg_label.Nolabel}[Nolabel]},
            - [~l:T -> CT]
                     when [lbl] is {{!Asttypes.arg_label.Labelled}[Labelled l]},
            - [?l:T -> CT]
                     when [lbl] is {{!Asttypes.arg_label.Optional}[Optional l]}.
         *)
  | Pcty_extension of extension  (** [%id] *)
  | Pcty_open of open_description * class_type  (** [let open M in CT] *)

[@@deriving  yojson]
and class_signature =
    {
     pcsig_self: core_type;
     pcsig_fields: class_type_field list;
    }
(** Values of type [class_signature] represents:
    - [object('selfpat) ... end]
    - [object ... end] when {{!class_signature.pcsig_self}[pcsig_self]}
                         is {{!core_type_desc.Ptyp_any}[Ptyp_any]}
*)

[@@deriving  yojson]
and class_type_field =
    {
     pctf_desc: class_type_field_desc;
     pctf_loc: Location.t;
     pctf_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
    }

[@@deriving  yojson]
and class_type_field_desc =
  | Pctf_inherit of class_type  (** [inherit CT] *)
  | Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)
      (** [val x: T] *)
  | Pctf_method of (label loc * private_flag * virtual_flag * core_type)
      (** [method x: T]

            Note: [T] can be a {{!core_type_desc.Ptyp_poly}[Ptyp_poly]}.
        *)
  | Pctf_constraint of (core_type * core_type)  (** [constraint T1 = T2] *)
  | Pctf_attribute of attribute  (** [[\@\@\@id]] *)
  | Pctf_extension of extension  (** [[%%id]] *)

[@@deriving  yojson]
and 'a class_infos =
    {
     pci_virt: virtual_flag;
     pci_params: (core_type * (variance * injectivity)) list;
     pci_name: string loc;
     pci_expr: 'a;
     pci_loc: Location.t;
     pci_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
    }
(** Values of type [class_expr class_infos] represents:
    - [class c = ...]
    - [class ['a1,...,'an] c = ...]
    - [class virtual c = ...]

   They are also used for "class type" declaration.
*)

[@@deriving  yojson]
and class_description = class_type class_infos

[@@deriving  yojson]
and class_type_declaration = class_type class_infos
 
end
