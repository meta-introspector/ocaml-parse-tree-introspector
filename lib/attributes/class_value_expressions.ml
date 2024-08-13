
module type ClassValueExpression = sig
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

type class_expr =
    {
     pcl_desc: class_expr_desc;
     pcl_loc: Location.t;
     pcl_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
    }

[@@deriving  yojson]
and class_expr_desc =
  | Pcl_constr of Longident.t loc * core_type list
      (** [c] and [['a1, ..., 'an] c] *)
  | Pcl_structure of class_structure  (** [object ... end] *)
  | Pcl_fun of arg_label * expression option * pattern * class_expr
      (** [Pcl_fun(lbl, exp0, P, CE)] represents:
            - [fun P -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Nolabel}[Nolabel]}
                      and [exp0] is [None],
            - [fun ~l:P -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Labelled}[Labelled l]}
                      and [exp0] is [None],
            - [fun ?l:P -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Optional}[Optional l]}
                      and [exp0] is [None],
            - [fun ?l:(P = E0) -> CE]
                     when [lbl]  is {{!Asttypes.arg_label.Optional}[Optional l]}
                      and [exp0] is [Some E0].
        *)
  | Pcl_apply of class_expr * (arg_label * expression) list
      (** [Pcl_apply(CE, [(l1,E1) ; ... ; (ln,En)])]
            represents [CE ~l1:E1 ... ~ln:En].
            [li] can be empty (non labeled argument) or start with [?]
            (optional argument).

            Invariant: [n > 0]
        *)
  | Pcl_let of rec_flag * value_binding list * class_expr
      (** [Pcl_let(rec, [(P1, E1); ... ; (Pn, En)], CE)] represents:
            - [let P1 = E1 and ... and Pn = EN in CE]
                when [rec] is {{!Asttypes.rec_flag.Nonrecursive}[Nonrecursive]},
            - [let rec P1 = E1 and ... and Pn = EN in CE]
                when [rec] is {{!Asttypes.rec_flag.Recursive}[Recursive]}.
        *)
  | Pcl_constraint of class_expr * class_type  (** [(CE : CT)] *)
  | Pcl_extension of extension  (** [[%id]] *)
  | Pcl_open of open_description * class_expr  (** [let open M in CE] *)
                                     [@@deriving  yojson]

and class_structure =
    {
     pcstr_self: pattern;
     pcstr_fields: class_field list;
    }
(** Values of type {!class_structure} represents:
    - [object(selfpat) ... end]
    - [object ... end] when {{!class_structure.pcstr_self}[pcstr_self]}
                         is {{!pattern_desc.Ppat_any}[Ppat_any]}
*)

[@@deriving  yojson]
and class_field =
    {
     pcf_desc: class_field_desc;
     pcf_loc: Location.t;
     pcf_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
    }

and class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string loc option
      (** [Pcf_inherit(flag, CE, s)] represents:
            - [inherit CE]
                    when [flag] is {{!Asttypes.override_flag.Fresh}[Fresh]}
                     [@@deriving  yojson]
and [s] is [None],
            - [inherit CE as x]
                   when [flag] is {{!Asttypes.override_flag.Fresh}[Fresh]}
                    [@@deriving  yojson]
and [s] is [Some x],
            - [inherit! CE]
                   when [flag] is {{!Asttypes.override_flag.Override}[Override]}
                    and [s] is [None],
            - [inherit! CE as x]
                   when [flag] is {{!Asttypes.override_flag.Override}[Override]}
                    and [s] is [Some x]
  *)
  | Pcf_val of (label loc * mutable_flag * class_field_kind)
      (** [Pcf_val(x,flag, kind)] represents:
            - [val x = E]
       when [flag] is {{!Asttypes.mutable_flag.Immutable}[Immutable]}
        and [kind] is {{!class_field_kind.Cfk_concrete}[Cfk_concrete(Fresh, E)]}
            - [val virtual x: T]
       when [flag] is {{!Asttypes.mutable_flag.Immutable}[Immutable]}
        and [kind] is {{!class_field_kind.Cfk_virtual}[Cfk_virtual(T)]}
            - [val mutable x = E]
       when [flag] is {{!Asttypes.mutable_flag.Mutable}[Mutable]}
        and [kind] is {{!class_field_kind.Cfk_concrete}[Cfk_concrete(Fresh, E)]}
            - [val mutable virtual x: T]
       when [flag] is {{!Asttypes.mutable_flag.Mutable}[Mutable]}
        and [kind] is {{!class_field_kind.Cfk_virtual}[Cfk_virtual(T)]}
  *)
  | Pcf_method of (label loc * private_flag * class_field_kind)
      (** - [method x = E]
                        ([E] can be a {{!expression_desc.Pexp_poly}[Pexp_poly]})
            - [method virtual x: T]
                        ([T] can be a {{!core_type_desc.Ptyp_poly}[Ptyp_poly]})
  *)
  | Pcf_constraint of (core_type * core_type)  (** [constraint T1 = T2] *)
  | Pcf_initializer of expression  (** [initializer E] *)
  | Pcf_attribute of attribute  (** [[\@\@\@id]] *)
  | Pcf_extension of extension  (** [[%%id]] *)

[@@deriving  yojson]
and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

[@@deriving  yojson]
and class_declaration = class_expr class_infos

end
